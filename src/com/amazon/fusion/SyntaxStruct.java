// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.cloneIfContained;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

final class SyntaxStruct
    extends SyntaxContainer
{
    /**
     * For repeated fields, the value is SyntaxValue[] otherwise it's
     * SyntaxValue.
     */
    private final Map<String, Object> myMap;


    /**
     * @param map may be null
     */
    private SyntaxStruct(Map<String, Object> map, String[] anns,
                         SourceLocation loc)
    {
        super(anns, loc);
        myMap = map;
    }

    static SyntaxStruct read(IonReader source, String[] anns)
    {
        SourceLocation loc = currentLocation(source);

        Map<String, Object> map;

        if (source.isNullValue())
        {
            map = null;
        }
        else
        {
            map = new HashMap<String, Object>();
            source.stepIn();
            while (source.next() != null)
            {
                String field = source.getFieldName();
                SyntaxValue child = Syntax.read(source);
                Object prev = map.put(field, child);
                if (prev != null)
                {
                    SyntaxValue[] multi;
                    if (prev instanceof SyntaxValue)
                    {
                        // Shifting from single to repeated field.
                        multi = new SyntaxValue[] { (SyntaxValue) prev, child };
                    }
                    else
                    {
                        SyntaxValue[] prevArray = (SyntaxValue[]) prev;
                        int len = prevArray.length;
                        multi = Arrays.copyOf(prevArray, len);
                        multi[len] = child;
                    }
                    map.put(field, multi);
                }
            }
            source.stepOut();
        }

        return new SyntaxStruct(map, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.STRUCT;
    }


    SyntaxValue get(String fieldName)
    {
        Object result = myMap.get(fieldName);
        if (result == null) return null;
        if (result instanceof SyntaxValue)
        {
            return (SyntaxValue) result;
        }

        return ((SyntaxValue[]) result)[0];
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        ValueFactory vf = eval.getSystem();
        IonStruct resultDom;
        if (isNullValue())
        {
            resultDom = vf.newNullStruct();
        }
        else
        {
            resultDom = vf.newEmptyStruct();
            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();
                Object value = entry.getValue();
                if (value instanceof SyntaxValue)
                {
                    SyntaxValue child = (SyntaxValue) value;
                    IonValue childDom = quoteChild(eval, child);
                    resultDom.add(fieldName, childDom);
                }
                else
                {
                    SyntaxValue[] children = (SyntaxValue[]) value;
                    for (SyntaxValue child : children)
                    {
                        IonValue childDom = quoteChild(eval, child);
                        resultDom.add(fieldName, childDom);
                    }
                }
            }
        }
        resultDom.setTypeAnnotations(getAnnotations());
        return new DomValue(resultDom);
    }


    private IonValue quoteChild(Evaluator eval, SyntaxValue child)
    {
        FusionValue childValue = child.quote(eval);
        return FusionValue.toIonValue(childValue);
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
        ValueFactory vf = eval.getSystem();
        IonStruct resultDom;
        if (isNullValue())
        {
            resultDom = vf.newNullStruct();
        }
        else
        {
            resultDom = vf.newEmptyStruct();
            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();
                Object value = entry.getValue();
                if (value instanceof SyntaxValue)
                {
                    SyntaxValue child = (SyntaxValue) value;
                    FusionValue childValue = eval.eval(env, child);
                    IonValue childDom = FusionValue.toIonValue(childValue);
                    childDom = cloneIfContained(childDom);
                    resultDom.add(fieldName, childDom);
                }
                else
                {
                    SyntaxValue[] children = (SyntaxValue[]) value;
                    for (SyntaxValue child : children)
                    {
                        FusionValue childValue = eval.eval(env, child);
                        IonValue childDom = FusionValue.toIonValue(childValue);
                        childDom = cloneIfContained(childDom);
                        resultDom.add(fieldName, childDom);
                    }
                }
            }
        }
        return new DomValue(resultDom);
    }


    @Override
    void writeContentTo(IonWriter writer) throws IOException
    {
        if (isNullValue())
        {
            writer.writeNull(IonType.STRUCT);
        }
        else
        {
            writer.stepIn(IonType.STRUCT);
            for (Map.Entry<String, Object> entry : myMap.entrySet())
            {
                String fieldName = entry.getKey();
                writer.setFieldName(fieldName);

                Object value = entry.getValue();
                if (value instanceof SyntaxValue)
                {
                    SyntaxValue child = (SyntaxValue) value;
                    child.writeTo(writer);
                }
                else
                {
                    SyntaxValue[] children = (SyntaxValue[]) value;
                    for (SyntaxValue child : children)
                    {
                        child.writeTo(writer);
                    }
                }
            }
            writer.stepOut();
        }
    }


    @Override
    boolean isNullValue()
    {
        return myMap == null;
    }
}
