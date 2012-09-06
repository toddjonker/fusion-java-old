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
     * <p>
     * <em>DO NOT MUTATE THIS OBJECT AND ITS CONTENTS AFTER CONSTRUCTION!</em>
     */
    private final Map<String, Object> myMap;


    /**
     * @param map may be null but may only be non-null when map has children.
     */
    private SyntaxStruct(Map<String, Object> map, String[] anns,
                         SourceLocation loc, SyntaxWraps wraps)
    {
        super(anns, loc, wraps);
        assert (wraps == null) || (map != null && ! map.isEmpty());
        myMap = map;
    }


    @Override
    boolean isNullValue()
    {
        return myMap == null;
    }


    @Override
    boolean hasNoChildren()
    {
        return myMap == null || myMap.isEmpty();
    }


    @Override
    SyntaxStruct copyReplacingWraps(SyntaxWraps wraps)
    {
        // We can share the Map because its never mutated.
        return new SyntaxStruct(myMap, getAnnotations(), getLocation(), wraps);
    }


    @Override
    SyntaxStruct stripWraps()
    {
        if (hasNoChildren()) return this;  // No children, no marks, all okay!

        // Even if we have no marks, some children may have them.
        boolean mustReplace = (myWraps != null);

        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Map<String, Object> newMap = new HashMap<String, Object>(myMap);

        for (Map.Entry<String, Object> entry : newMap.entrySet())
        {
            Object value = entry.getValue();
            if (value instanceof SyntaxValue)
            {
                SyntaxValue child = (SyntaxValue) value;
                SyntaxValue stripped = child.stripWraps();
                if (stripped != child)
                {
                    entry.setValue(stripped);
                    mustReplace = true;
                }
            }
            else
            {
                SyntaxValue[] children = (SyntaxValue[]) value;
                int childCount = children.length;

                boolean mustReplaceArray = false;
                SyntaxValue[] newChildren = new SyntaxValue[childCount];
                for (int i = 0; i < childCount; i++)
                {
                    SyntaxValue child = children[i];
                    SyntaxValue stripped = child.stripWraps();
                    if (stripped != child)
                    {
                        newChildren[i] = stripped;
                        mustReplaceArray = true;
                    }
                }

                if (mustReplaceArray)
                {
                    entry.setValue(newChildren);
                    mustReplace = true;
                }
            }
        }

        if (! mustReplace) return this;

        return new SyntaxStruct(newMap, getAnnotations(), getLocation(), null);
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

        return new SyntaxStruct(map, anns, loc, null);
    }


    @Override
    Type getType()
    {
        return Type.STRUCT;
    }


    SyntaxValue get(String fieldName)
    {
        // This should only be called at runtime, after wraps are pushed.
        assert myWraps == null;

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
        // TODO quote should strip wraps at compile time
        // This should only be called at runtime, after wraps are pushed.
        //assert myWraps == null;

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
    SyntaxValue prepare(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        Map<String, Object> newMap = null;
        if (myMap != null)
        {
            // Make a copy of the map, then mutate it to replace children
            // as necessary.
            newMap = new HashMap<String, Object>(myMap);

            for (Map.Entry<String, Object> entry : newMap.entrySet())
            {
                Object value = entry.getValue();
                if (value instanceof SyntaxValue)
                {
                    SyntaxValue subform = (SyntaxValue) value;
                    if (myWraps != null)
                    {
                        subform = subform.addWraps(myWraps);
                    }
                    subform = subform.prepare(eval, env);
                    entry.setValue(subform);
                }
                else
                {
                    SyntaxValue[] children = (SyntaxValue[]) value;
                    int childCount = children.length;

                    SyntaxValue[] newChildren = new SyntaxValue[childCount];
                    for (int i = 0; i < childCount; i++)
                    {
                        SyntaxValue subform = children[i];
                        if (myWraps != null)
                        {
                            subform = subform.addWraps(myWraps);
                        }
                        newChildren[i] = subform.prepare(eval, env);
                    }
                    entry.setValue(newChildren);
                }
            }
        }

        // Wraps have been pushed down so the copy doesn't need them.
        return new SyntaxStruct(newMap, getAnnotations(), getLocation(), null);
    }

    @Override
    FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
        // This should only be called at runtime, after wraps are pushed.
        assert myWraps == null;

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
}
