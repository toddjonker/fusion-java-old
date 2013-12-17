// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBlob.makeBlob;
import static com.amazon.fusion.FusionBool.isBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionClob.makeClob;
import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionList.isList;
import static com.amazon.fusion.FusionList.nullList;
import static com.amazon.fusion.FusionList.unsafeListRef;
import static com.amazon.fusion.FusionList.unsafeListSize;
import static com.amazon.fusion.FusionLob.isLob;
import static com.amazon.fusion.FusionNull.isNullNull;
import static com.amazon.fusion.FusionNumber.isFloat;
import static com.amazon.fusion.FusionNumber.isIntOrDecimal;
import static com.amazon.fusion.FusionNumber.makeDecimal;
import static com.amazon.fusion.FusionNumber.makeFloat;
import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionSexp.isEmptySexp;
import static com.amazon.fusion.FusionSexp.isPair;
import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionSexp.pair;
import static com.amazon.fusion.FusionSexp.unsafePairHead;
import static com.amazon.fusion.FusionSexp.unsafePairTail;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionStruct.isStruct;
import static com.amazon.fusion.FusionSymbol.isSymbol;
import static com.amazon.fusion.FusionSymbol.unsafeSymbolToJavaString;
import static com.amazon.fusion.FusionTimestamp.isTimestamp;
import static com.amazon.fusion.FusionTimestamp.makeTimestamp;
import static com.amazon.fusion.FusionValue.annotationsAsJavaStrings;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.fusion.FusionList.BaseList;
import com.amazon.fusion.FusionSexp.BaseSexp;
import com.amazon.fusion.FusionStruct.BaseStruct;
import com.amazon.fusion.FusionStruct.StructFieldVisitor;
import com.amazon.ion.Decimal;
import com.amazon.ion.IonException;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;
import com.amazon.ion.Timestamp;
import java.math.BigInteger;
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
     * Reads a single Ion datum as source.
     *
     * @param source must be positioned on the value to be read.
     * @param name may be null.
     * @throws IonException if there's a problem reading the source data.
     */
    static SyntaxValue read(Evaluator eval, IonReader source, SourceName name)
        throws IonException
    {
        IonType type = source.getType();
        assert type != null;

        String[] anns = source.getTypeAnnotations();
        SourceLocation loc = currentLocation(source, name);

        BaseValue datum;
        switch (type)
        {
            case NULL:
            {
                datum = FusionNull.makeNullNull(eval, anns);
                break;
            }
            case BOOL:
            {
                if (source.isNullValue())
                {
                    datum = makeBool(eval, anns, (Boolean) null);
                }
                else
                {
                    boolean value = source.booleanValue();
                    datum = makeBool(eval, anns, value);
                }
                break;
            }
            case INT:
            {
                BigInteger value = source.bigIntegerValue();
                datum = makeInt(eval, anns, value);
                break;
            }
            case DECIMAL:
            {
                Decimal value = source.decimalValue();
                datum = makeDecimal(eval, anns, value);
                break;
            }
            case FLOAT:
            {
                if (source.isNullValue())
                {
                    datum = makeFloat(eval, anns, (Double) null);
                }
                else
                {
                    double value = source.doubleValue();
                    datum = makeFloat(eval, anns, value);
                }
                break;
            }
            case TIMESTAMP:
            {
                Timestamp value = source.timestampValue();
                datum = makeTimestamp(eval, anns, value);
                break;
            }
            case STRING:
            {
                String value = source.stringValue();
                return SyntaxString.make(eval, loc, anns, value);
            }
            case SYMBOL:
            {
                String value = source.stringValue();
                if (value != null &&
                    value.startsWith("_") &&
                    value.endsWith("_"))
                {
                    return SyntaxKeyword.make(eval, loc, anns, value);
                }
                return SyntaxSymbol.make(eval, loc, anns, value);
            }
            case BLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                datum = makeBlob(eval, anns, value);
                break;
            }
            case CLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                datum = makeClob(eval, anns, value);
                break;
            }
            case LIST:
            {
                datum = readList(eval, name, source, anns);
                return SyntaxList.make(eval, loc, datum);
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
            default:
            {
                throw new UnsupportedOperationException("Bad type: " + type);
            }
        }

        return makeSyntax(eval, loc, datum);
    }


    /**
     * @param source must be positioned on the value to be read, but not
     * stepped-in.
     * @param name may be null.
     *
     * @return null if the sequence is null (eg, {@code null.list}).
     */
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


    /**
     * @return an immutable list of syntax objects.
     */
    static BaseList readList(Evaluator  eval,
                             SourceName name,
                             IonReader  source,
                             String[]   annotations)
    {
        if (source.isNullValue()) return nullList(eval, annotations);

        Object[] elements = readSequence(eval, source, name);
        return immutableList(eval, annotations, elements);
    }


    /**
     * @param context may be null, in which case nothing happens.
     */
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
     * TODO FUSION-242 This needs to do cycle detection.
     *
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

        if (isPair(eval, datum))
        {
            return (SyntaxValue) pairToStrippedSyntaxMaybe(eval, datum, true);
        }

        if (isSexp(eval, datum))  // null.sexp or ()
        {
            return SyntaxSexp.make(eval, null, (BaseSexp) datum);
        }

        if (isSymbol(eval, datum))
        {
            String value = unsafeSymbolToJavaString(eval, datum);
            if (value != null &&
                value.startsWith("_") &&
                value.endsWith("_"))
            {
                return SyntaxKeyword.make(eval, /*location*/ null, datum);
            }
            return SyntaxSymbol.make(eval, /*location*/ null, datum);
        }

        if (isList(eval, datum))
        {
            int size = unsafeListSize(eval, datum);
            if (size == 0)
            {
                return SyntaxList.make(eval, null, datum);
            }

            Object[] children = new Object[size];
            for (int i = 0; i < size; i++)
            {
                Object rawChild = unsafeListRef(eval, datum, i);
                Object child = datumToStrippedSyntaxMaybe(eval, rawChild);
                if (child == null)
                {
                    // Hit something that's not syntax-able
                    return null;
                }
                children[i] = child;
            }

            String[] anns = annotationsAsJavaStrings(eval, datum);
            Object list = immutableList(eval, anns, children);
            return SyntaxList.make(eval, null, list);
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
                datum = ((BaseStruct) datum).transformFields(visitor);
                return SyntaxStruct.make(eval, /*location*/ null, datum);
            }
            catch (StripFailure e)  // This is crazy.
            {
                return null;
            }
        }

        if (isString(eval, datum))
        {
            return SyntaxString.make(eval, /*location*/ null, datum);
        }

        if (isIntOrDecimal(eval, datum) ||
            isBool(eval, datum)         ||
            isTimestamp(eval, datum)    ||
            isNullNull(eval, datum)     ||
            isFloat(eval, datum)        ||
            isLob(eval, datum))
        {
            return makeSyntax(eval, /*location*/ null, datum);
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
     * @param whosCalling The form to name for error messages; may be null.
     *
     * @return not null.
     */
    static SyntaxValue datumToSyntax(Evaluator eval,
                                     Object datum,
                                     SyntaxSymbol context,
                                     String whosCalling)
        throws FusionException
    {
        SyntaxValue stx = datumToSyntaxMaybe(eval, datum, context);
        if (stx == null)
        {
            String message =
                (whosCalling == null ? "datum_to_syntax" : whosCalling) +
                " expects syntax object or ionizable data, given " + datum;
            throw new ContractException(message);
        }

        return stx;
    }

    /**
     * @param context may be null, in which case no lexical information is
     * applied (and any existing is stripped).
     *
     * @return not null.
     */
    static SyntaxValue datumToSyntax(Evaluator eval,
                                     Object datum,
                                     SyntaxSymbol context)
        throws FusionException
    {
        return datumToSyntax(eval, datum, context, null);
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
