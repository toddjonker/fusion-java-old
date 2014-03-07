// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionList.nullList;
import static com.amazon.fusion.FusionNumber.makeDecimal;
import static com.amazon.fusion.FusionNumber.makeFloat;
import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionSexp.immutableSexp;
import static com.amazon.fusion.FusionSexp.nullSexp;
import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.FusionStruct.immutableStruct;
import static com.amazon.fusion.FusionStruct.nullStruct;
import static com.amazon.fusion.FusionStruct.structImplAdd;
import static com.amazon.fusion.FusionSymbol.makeSymbol;
import static com.amazon.fusion.FusionTimestamp.makeTimestamp;
import static com.amazon.fusion.SourceLocation.forCurrentSpan;
import com.amazon.fusion.FusionList.BaseList;
import com.amazon.fusion.FusionSexp.BaseSexp;
import com.amazon.ion.Decimal;
import com.amazon.ion.IonException;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;
import com.amazon.ion.Timestamp;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

/**
 *
 */
class StandardReader
{
    /**
     * Reads a single Ion datum.
     *
     * @param source must be positioned on the value to be read.
     */
    static Object read(Evaluator eval,
                       IonReader source)
        throws FusionException
    {
        try
        {
            return read(eval, source, null, false);
        }
        catch (IonException e)
        {
            throw new FusionException("Error reading data: " + e.getMessage(),
                                      e);
        }
    }


    /**
     * Reads a single Ion datum as source.
     *
     * @param source must be positioned on the value to be read.
     * @param name may be null.
     */
    static SyntaxValue readSyntax(Evaluator  eval,
                                  IonReader  source,
                                  SourceName name)
        throws FusionException
    {
        try
        {
            return (SyntaxValue) read(eval, source, name, true);
        }
        catch (IonException e)
        {
            throw new FusionException("Error reading source: " + e.getMessage(),
                                      e);
        }
    }


    /**
     * Reads a single Ion datum, optionally as syntax objects.
     * @param source must be positioned on the value to be read.
     * @param name may be null.
     *
     * @throws IonException if there's a problem reading the source data.
     */
    private static Object read(Evaluator  eval,
                               IonReader  source,
                               SourceName name,
                               boolean    readingSyntax)
        throws FusionException, IonException
    {
        IonType type = source.getType();
        assert type != null;

        String[] anns = source.getTypeAnnotations();
        SourceLocation loc =
            (readingSyntax ? forCurrentSpan(source, name) : null);

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
                datum = makeString(eval, anns, value);
                break;
            }
            case SYMBOL:
            {
                String value = source.stringValue();
                datum = makeSymbol(eval, anns, value);
                break;
            }
            case BLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                datum = FusionBlob.forBytesNoCopy(eval, anns, value);
                break;
            }
            case CLOB:
            {
                byte[] value =
                    (source.isNullValue() ? null : source.newBytes());
                datum = FusionClob.forBytesNoCopy(eval, anns, value);
                break;
            }
            case LIST:
            {
                datum = readList(eval, source, name, readingSyntax, anns);
                break;
            }
            case SEXP:
            {
                datum = readSexp(eval, source, name, readingSyntax, anns);
                break;
            }
            case STRUCT:
            {
                datum = readStruct(eval, source, name, readingSyntax, anns);
                break;
            }
            default:
            {
                throw new UnsupportedOperationException("Bad type: " + type);
            }
        }

        if (readingSyntax)
        {
            return datum.wrapAsSyntax(eval, loc);
        }

        return datum;
    }



    /**
     * @param source must be positioned on the value to be read, but not
     * stepped-in.
     * @param name may be null.
     * @return null if the sequence is null (eg, {@code null.list}).
     *
     * @throws IonException if there's a problem reading the source data.
     */
    private static Object[] readSequence(Evaluator  eval,
                                         IonReader  source,
                                         SourceName name,
                                         boolean    readingSyntax)
        throws FusionException, IonException
    {
        if (source.isNullValue()) return null;

        ArrayList<Object> children = new ArrayList<>();

        source.stepIn();
        while (source.next() != null)
        {
            Object child = read(eval, source, name, readingSyntax);
            children.add(child);
        }
        source.stepOut();

        return children.toArray();
    }


    /**
     * @return an immutable list of syntax objects.
     *
     * @throws IonException if there's a problem reading the source data.
     */
    private static BaseList readList(Evaluator  eval,
                                     IonReader  source,
                                     SourceName name,
                                     boolean    readingSyntax,
                                     String[]   annotations)
        throws FusionException, IonException
    {
        if (source.isNullValue())
        {
            return nullList(eval, annotations);
        }

        Object[] elements = readSequence(eval, source, name, readingSyntax);
        return immutableList(eval, annotations, elements);
    }


    /**
     * @throws IonException if there's a problem reading the source data.
     */
    private static BaseSexp readSexp(Evaluator  eval,
                                     IonReader  source,
                                     SourceName name,
                                     boolean    readingSyntax,
                                     String[]   annotations)
        throws FusionException, IonException
    {
        if (source.isNullValue())
        {
            return nullSexp(eval, annotations);
        }

        Object[] elements = readSequence(eval, source, name, readingSyntax);
        return immutableSexp(eval, annotations, elements);
    }


    /**
     * @throws IonException if there's a problem reading the source data.
     */
    private static BaseValue readStruct(Evaluator  eval,
                                        IonReader  source,
                                        SourceName name,
                                        boolean    readingSyntax,
                                        String[]   anns)
        throws FusionException, IonException
    {
        if (source.isNullValue())
        {
            return nullStruct(eval, anns);
        }

        Map<String, Object> map = new HashMap<String, Object>();
        source.stepIn();
        while (source.next() != null)
        {
            String field = source.getFieldName();
            Object child = read(eval, source, name, readingSyntax);
            structImplAdd(map, field, child);
        }
        source.stepOut();

        return immutableStruct(map, anns);
    }
}
