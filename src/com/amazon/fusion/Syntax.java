// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.castToIonValueMaybe;
import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorRef;
import static com.amazon.fusion.FusionVector.unsafeVectorSize;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.ion.Decimal;
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

    static SyntaxValue read(IonReader source, SourceName name)
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
                // TODO WORKAROUND ION-290 this check shouldn't be needed
                Decimal value =
                    (source.isNullValue() ? null : source.decimalValue());
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
                SyntaxValue[] value = readSequence(source, name);
                return SyntaxList.make(value, anns, loc);
            }
            case SEXP:
            {
                SyntaxValue[] value = readSequence(source, name);
                return SyntaxSexp.make(value, anns, loc);
            }
            case STRUCT:
            {
                return SyntaxStruct.read(source, name, anns);
            }
        }

        throw new UnsupportedOperationException("Bad type: " + type);
    }


    static SyntaxValue[] readSequence(IonReader source, SourceName name)
    {
        if (source.isNullValue()) return null;

        ArrayList<SyntaxValue> children = new ArrayList<SyntaxValue>();

        source.stepIn();
        while (source.next() != null)
        {
            SyntaxValue child = Syntax.read(source, name);
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
    {
        if (context != null)
        {
            datum = context.copyWrapsTo(datum);
        }
        return datum;
    }

    /**
     * @param context may be null
     */
    static SyntaxValue datumToSyntax(Evaluator eval,
                                     SyntaxSymbol context,
                                     SyntaxValue datum)
        throws FusionException
    {
        // TODO Should strip location and properties
        datum = datum.stripWraps();
        return applyContext(eval, context, datum);
    }


    /**
     * We apply the context only once at the top, so it propagates lazily.
     */
    private static SyntaxValue datumToStrippedSyntaxMaybe(Evaluator eval,
                                                          SyntaxSymbol context,
                                                          Object datum)
        throws FusionException
    {
        if (isSyntax(eval, datum))
        {
            // TODO Should strip location and properties
            return ((SyntaxValue) datum).stripWraps();
        }

        IonValue iv = castToIonValueMaybe(datum);
        if (iv != null)
        {
            IonReader r = eval.getSystem().newReader(iv);
            r.next();
            return read(r, null);
            // No need to strip wraps here
        }

        if (isVector(eval, datum))
        {
            int size = unsafeVectorSize(eval, datum);
            SyntaxValue[] children = new SyntaxValue[size];
            for (int i = 0; i < size; i++)
            {
                Object rawChild = unsafeVectorRef(eval, datum, i);
                SyntaxValue child =
                    datumToStrippedSyntaxMaybe(eval, context, rawChild);
                if (child == null)
                {
                    // Hit something that's not syntax-able
                    return null;
                }
                children[i] = child;
            }
            return SyntaxList.make(null, children);
        }

        return null;
    }

    static SyntaxValue datumToSyntaxMaybe(Evaluator eval,
                                          SyntaxSymbol context,
                                          Object datum)
        throws FusionException
    {
        if (isSyntax(eval, datum))
        {
            return datumToSyntax(eval, context, (SyntaxValue) datum);
        }

        SyntaxValue stx = datumToStrippedSyntaxMaybe(eval, context, datum);
        if (stx == null) return null;

        return applyContext(eval, context, stx);
    }

    static SyntaxValue datumToSyntax(Evaluator eval,
                                     SyntaxSymbol context,
                                     Object datum)
        throws FusionException
    {
        SyntaxValue stx = datumToSyntaxMaybe(eval, context, datum);
        if (stx == null)
        {
            throw new ArgTypeFailure("datum_to_syntax",
                                     "Syntax object or Ionizable data",
                                     1, context, datum);
        }

        return stx;
    }
}
