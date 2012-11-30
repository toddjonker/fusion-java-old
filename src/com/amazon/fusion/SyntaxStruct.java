// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.EMPTY_STRUCT;
import static com.amazon.fusion.FusionStruct.NULL_STRUCT;
import static com.amazon.fusion.FusionStruct.immutableStruct;
import static com.amazon.fusion.FusionStruct.nullStruct;
import static com.amazon.fusion.FusionStruct.structImplAdd;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
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


    @Deprecated // Due to bug noted within. Note to myself to fix before
                // adding a use case that may fall prey to it.
    static SyntaxStruct make(String[] names, SyntaxValue[] values,
                             String[] anns)
    {
        Map<String, Object> map = new HashMap<String, Object>(names.length);
        for (int i = 0; i < names.length; i++)
        {
            // FIXME wrong for multi-value
            Object prev = map.put(names[i], values[i]);
            if (prev != null)
            {
                throw new AssertionError("Error handling repeated fields in syntax");
            }
        }

        return new SyntaxStruct(map, anns, null, null);
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


    static SyntaxStruct read(IonReader source, SourceName name, String[] anns)
    {
        SourceLocation loc = currentLocation(source, name);

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
                SyntaxValue child = Syntax.read(source, name);
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
                        multi = Arrays.copyOf(prevArray, len+1);
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
    Object quote(Evaluator eval)
        throws FusionException
    {
        // This should only be called at runtime, after wraps are pushed.
        assert myWraps == null;

        String[] annotations = getAnnotations();

        if (isNullValue())
        {
            return nullStruct(eval, annotations);
        }

        String[] names  = new String[myMap.size()];
        Object[] values = new Object[myMap.size()];

        int pos = 0;
        for (Map.Entry<String, Object> entry : myMap.entrySet())
        {
            String fieldName = entry.getKey();
            Object value = entry.getValue();
            if (value instanceof SyntaxValue)
            {
                SyntaxValue child = (SyntaxValue) value;
                Object childValue = child.quote(eval);
                names[pos]  = fieldName;
                values[pos] = childValue;
            }
            else
            {
                SyntaxValue[] children = (SyntaxValue[]) value;
                Object[] childValues = new Object[children.length];

                int cPos = 0;
                for (SyntaxValue child : children)
                {
                    childValues[cPos++] = child.quote(eval);
                }

                names[pos]  = fieldName;
                values[pos] = childValues;
            }

            pos++;
        }

        return immutableStruct(names, values, annotations);
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env)
        throws FusionException
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
                    subform = subform.expand(eval, env);
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
                        newChildren[i] = subform.expand(eval, env);
                    }
                    entry.setValue(newChildren);
                }
            }
        }

        // Wraps have been pushed down so the copy doesn't need them.
        return new SyntaxStruct(newMap, getAnnotations(), getLocation(), null);
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, FusionException
    {
        ionizeAnnotations(writer);

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

                Object value = entry.getValue();
                if (value instanceof SyntaxValue)
                {
                    SyntaxValue child = (SyntaxValue) value;
                    writer.setFieldName(fieldName);
                    child.ionize(eval, writer);
                }
                else
                {
                    SyntaxValue[] children = (SyntaxValue[]) value;
                    for (SyntaxValue child : children)
                    {
                        writer.setFieldName(fieldName);
                        child.ionize(eval, writer);
                    }
                }
            }
            writer.stepOut();
        }
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        assert myWraps == null;

        if (isNullValue())
        {
            return new CompiledConstant(NULL_STRUCT);
        }

        if (myMap.size() == 0)
        {
            return new CompiledConstant(EMPTY_STRUCT);
        }

        // Pre-compute the size so we can allocate arrays all at once.
        int size = 0;
        for (Object value : myMap.values())
        {
            if (value instanceof SyntaxValue)
            {
                size += 1;
            }
            else
            {
                SyntaxValue[] children = (SyntaxValue[]) value;
                size += children.length;
            }
        }

        String[]       fieldNames = new String[size];
        CompiledForm[] fieldForms = new CompiledForm[size];
        int i = 0;

        for (Map.Entry<String, Object> entry : myMap.entrySet())
        {
            String fieldName = entry.getKey();

            Object value = entry.getValue();
            if (value instanceof SyntaxValue)
            {
                SyntaxValue child = (SyntaxValue) value;
                CompiledForm form = eval.compile(env, child);

                fieldNames[i] = fieldName;
                fieldForms[i] = form;
                i++;
            }
            else
            {
                SyntaxValue[] children = (SyntaxValue[]) value;
                for (SyntaxValue child : children)
                {
                    CompiledForm form = eval.compile(env, child);

                    fieldNames[i] = fieldName;
                    fieldForms[i] = form;
                    i++;
                }
            }
        }
        assert i == size;

        return new CompiledStruct(fieldNames, fieldForms);
    }


    //========================================================================


    private static final class CompiledStruct
        implements CompiledForm
    {
        private final String[]       myFieldNames;
        private final CompiledForm[] myFieldForms;

        CompiledStruct(String[] fieldNames, CompiledForm[] fieldForms)
        {
            myFieldNames = fieldNames;
            myFieldForms = fieldForms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            HashMap<String, Object> map = new HashMap<String, Object>();

            for (int i = 0; i < myFieldNames.length; i++)
            {
                CompiledForm form = myFieldForms[i];
                Object value = eval.eval(store, form);

                String fieldName = myFieldNames[i];

                structImplAdd(map, fieldName, value);
            }

            return immutableStruct(map);
        }
    }
}
