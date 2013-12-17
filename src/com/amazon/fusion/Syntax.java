// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBlob.makeBlob;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionClob.makeClob;
import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionList.nullList;
import static com.amazon.fusion.FusionNumber.makeDecimal;
import static com.amazon.fusion.FusionNumber.makeFloat;
import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionTimestamp.makeTimestamp;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.fusion.FusionList.BaseList;
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


    /**
     * TODO FUSION-242 This needs to do cycle detection.
     *
     * @return null if something in the datum can't be converted into syntax.
     */
    static SyntaxValue datumToStrippedSyntaxMaybe(Evaluator eval, Object datum)
        throws FusionException
    {
        if (datum instanceof BaseValue)
        {
            return ((BaseValue) datum).toStrippedSyntaxMaybe(eval);
        }

        return null;
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
