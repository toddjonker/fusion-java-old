// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.isList;
import static com.amazon.fusion.FusionList.unsafeListRef;
import static com.amazon.fusion.FusionList.unsafeListSize;
import static com.amazon.fusion.FusionSexp.isEmptySexp;
import static com.amazon.fusion.FusionSexp.isPair;
import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionSexp.pair;
import static com.amazon.fusion.FusionSexp.unsafePairHead;
import static com.amazon.fusion.FusionSexp.unsafePairTail;
import static com.amazon.fusion.FusionStruct.isStruct;
import static com.amazon.fusion.FusionValue.castToIonValueMaybe;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.fusion.FusionSexp.BaseSexp;
import com.amazon.fusion.FusionStruct.BaseStruct;
import com.amazon.fusion.FusionStruct.ImmutableStruct;
import com.amazon.fusion.FusionStruct.StructFieldVisitor;
import com.amazon.ion.Decimal;
import com.amazon.ion.IonException;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.util.ArrayList;

/**
 * Utilities for working with {@link SyntaxValue}s.
 */
final class Syntax
{
    private Syntax() {}

    static boolean isSyntax(Evaluator eval, Object value)
    {
        return (value instanceof SyntaxValue);
    }

    static boolean isIdentifier(Evaluator eval, Object value)
    {
        return (value instanceof SyntaxSymbol);
    }

    /**
     * @throws IonException if there's a problem reading the source data.
     */
    static SyntaxValue read(Evaluator eval, IonReader source, SourceName name)
        throws IonException
    {
        IonType type = source.getType();
        assert type != null;

        String[] anns = source.getTypeAnnotations();
        SourceLocation loc = currentLocation(source, name);

        switch (type)
        {
            case NULL:
            {
                return SyntaxNull.make(anns, loc);
            }
            case BOOL:
            {
                Boolean value =
                    (source.isNullValue() ? null : source.booleanValue());
                return SyntaxBool.make(value, anns, loc);
            }
            case INT:
            {
                return SyntaxInt.make(source.bigIntegerValue(), anns, loc);
            }
            case DECIMAL:
            {
                Decimal value = source.decimalValue();
                return SyntaxDecimal.make(value, anns, loc);
            }
            case FLOAT:
            {
                Double value =
                    (source.isNullValue() ? null : source.doubleValue());
                return SyntaxFloat.make(value, anns, loc);
            }
            case TIMESTAMP:
            {
                Timestamp value = source.timestampValue();
                return SyntaxTimestamp.make(value, anns, loc);
            }
            case STRING:
            {
                String value = source.stringValue();
                return SyntaxString.make(value, anns, loc);
            }
            case SYMBOL:
            {
                String value = source.stringValue();
                if (value != null &&
                    value.startsWith("_") &&
                    value.endsWith("_"))
                {
                    return SyntaxKeyword.make(value, anns, loc);
                }
                return SyntaxSymbol.make(value, anns, loc);
            }
            case BLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                return SyntaxBlob.make(value, anns, loc);
            }
            case CLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                return SyntaxClob.make(value, anns, loc);
            }
            case LIST:
            {
                SyntaxValue[] value = readSequence(eval, source, name);
                return SyntaxList.make(loc, anns, value);
            }
            case SEXP:
            {
                SyntaxValue[] value = readSequence(eval, source, name);
                return SyntaxSexp.make(eval, loc, anns, value);
            }
            case STRUCT:
            {
                return SyntaxStruct.read(eval, source, name, anns);
            }
        }

        throw new UnsupportedOperationException("Bad type: " + type);
    }


    static SyntaxValue[] readSequence(Evaluator eval,
                                      IonReader source,
                                      SourceName name)
    {
        if (source.isNullValue()) return null;

        ArrayList<SyntaxValue> children = new ArrayList<SyntaxValue>();

        source.stepIn();
        while (source.next() != null)
        {
            SyntaxValue child = Syntax.read(eval, source, name);
            children.add(child);
        }
        source.stepOut();

        SyntaxValue[] childs = new SyntaxValue[children.size()];
        children.toArray(childs);
        return childs;
    }


    private static SyntaxValue applyContext(Evaluator eval,
                                            SyntaxSymbol context,
                                            SyntaxValue datum)
        throws FusionException
    {
        if (context != null)
        {
            datum = context.copyWrapsTo(datum);
        }
        return datum;
    }


    private static Object pairToStrippedSyntaxMaybe(Evaluator eval,
                                                    Object pair,
                                                    boolean first)
        throws FusionException
    {
        Object rawHead = unsafePairHead(eval, pair);
        Object rawTail = unsafePairTail(eval, pair);

        SyntaxValue newHead = datumToStrippedSyntaxMaybe(eval, rawHead);

        Object newTail;
        if (isPair(eval, rawTail))
        {
            newTail = pairToStrippedSyntaxMaybe(eval, rawTail, false);
        }
        else if (isEmptySexp(eval, rawTail))
        {
            newTail = rawTail;
        }
        else
        {
            newTail = datumToStrippedSyntaxMaybe(eval, rawTail);
        }

        if (newHead == null || newTail == null) return null;

        BaseSexp newPair = pair(eval, ((BaseSexp) pair).myAnnotations,
                                newHead, newTail);

        return (first ? SyntaxSexp.make(eval, null, newPair) : newPair);
    }


    /**
     * @return null if something in the datum can't be converted into syntax.
     */
    private static SyntaxValue
    datumToStrippedSyntaxMaybe(final Evaluator eval, Object datum)
        throws FusionException
    {
        if (isSyntax(eval, datum))
        {
            // TODO FUSION-183 Should strip location and properties?
            //      Well, probably not, that throws away existing
            //      context when called from datum_to_syntax
            return ((SyntaxValue) datum).stripWraps(eval);
        }

        IonValue iv = castToIonValueMaybe(datum);
        if (iv != null)
        {
            IonReader r = eval.getSystem().newReader(iv);
            r.next();
            return read(eval, r, null);
            // No need to strip wraps here
        }

        if (isList(eval, datum))
        {
            int size = unsafeListSize(eval, datum);
            SyntaxValue[] children = new SyntaxValue[size];
            for (int i = 0; i < size; i++)
            {
                Object rawChild = unsafeListRef(eval, datum, i);
                SyntaxValue child = datumToStrippedSyntaxMaybe(eval, rawChild);
                if (child == null)
                {
                    // Hit something that's not syntax-able
                    return null;
                }
                children[i] = child;
            }
            return SyntaxList.make(null, children);
        }

        if (isPair(eval, datum))
        {
            return (SyntaxValue) pairToStrippedSyntaxMaybe(eval, datum, true);
        }

        if (isSexp(eval, datum))  // null.sexp or ()
        {
            return SyntaxSexp.make(eval, null, (BaseSexp) datum);
        }

        if (isStruct(eval, datum))
        {
            StructFieldVisitor visitor = new StructFieldVisitor()
            {
                @Override
                public Object visit(String name, Object value)
                    throws FusionException
                {
                    SyntaxValue stripped =
                        datumToStrippedSyntaxMaybe(eval, value);
                    if (stripped == null)
                    {
                        // Hit something that's not syntax-able
                        throw new StripFailure();
                    }
                    return stripped;
                }
            };

            try
            {
                ImmutableStruct struct =
                    ((BaseStruct) datum).transformFields(visitor);
                return SyntaxStruct.make(struct, null, null);
            }
            catch (StripFailure e)  // This is crazy.
            {
                return null;
            }
        }

        return null;
    }

    @SuppressWarnings("serial")
    private static final class StripFailure extends RuntimeException
    {
    }


    /**
     * @param context may be null, in which case no lexical information is
     * applied (and any existing is stripped).
     *
     * @return null if something in the datum can't be converted into syntax.
     */
    static SyntaxValue datumToSyntaxMaybe(Evaluator eval,
                                          Object datum,
                                          SyntaxSymbol context)
        throws FusionException
    {
        if (isSyntax(eval, datum))
        {
            return datumToSyntax(eval, (SyntaxValue) datum, context);
        }

        SyntaxValue stx = datumToStrippedSyntaxMaybe(eval, datum);
        if (stx == null) return null;

        return applyContext(eval, context, stx);
    }


    /**
     * @param context may be null, in which case no lexical information is
     * applied (and any existing is stripped).
     */
    static SyntaxValue datumToSyntax(Evaluator eval,
                                     Object datum,
                                     SyntaxSymbol context)
        throws FusionException
    {
        SyntaxValue stx = datumToSyntaxMaybe(eval, datum, context);
        if (stx == null)
        {
            throw new ArgTypeFailure("datum_to_syntax",
                                     "Syntax object or Ionizable data",
                                     0, datum, context);
        }

        return stx;
    }

    /**
     * @param context may be null, in which case no lexical information is
     * applied (and any existing is stripped).
     */
    static SyntaxValue datumToSyntax(Evaluator eval,
                                     SyntaxValue datum,
                                     SyntaxSymbol context)
        throws FusionException
    {
        // TODO FUSION-183 Strip location and properties?
        datum = datum.stripWraps(eval);
        return applyContext(eval, context, datum);
    }
}
