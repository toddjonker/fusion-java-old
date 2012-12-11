// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionPrint.dispatchIonize;
import static com.amazon.fusion.FusionStruct.EMPTY_STRUCT;
import static com.amazon.fusion.FusionStruct.NULL_STRUCT;
import static com.amazon.fusion.FusionStruct.immutableStruct;
import static com.amazon.fusion.FusionStruct.nullStruct;
import static com.amazon.fusion.FusionStruct.structImplAdd;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.fusion.FusionStruct.ImmutableStruct;
import com.amazon.fusion.FusionStruct.NonNullImmutableStruct;
import com.amazon.fusion.FusionStruct.StructFieldVisitor;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

final class SyntaxStruct
    extends SyntaxContainer
{
    private final ImmutableStruct myStruct;


    /**
     * @param struct must not be null.
     */
    private SyntaxStruct(ImmutableStruct struct,
                         SourceLocation loc, SyntaxWraps wraps)
    {
        super(struct.myAnnotations, loc, wraps);
        assert (wraps == null) || (struct.size() != 0);
        myStruct = struct;
    }

    /**
     * @param map may be null but may only be non-null when map has children.
     */
    private SyntaxStruct(Map<String, Object> map, String[] anns,
                         SourceLocation loc, SyntaxWraps wraps)
    {
        super(anns, loc, wraps);
        assert (wraps == null) || (map != null && ! map.isEmpty());
        myStruct = immutableStruct(map, anns);
    }


    static SyntaxStruct make(ImmutableStruct struct,
                             SourceLocation loc, SyntaxWraps wraps)
    {
        return new SyntaxStruct(struct, loc, wraps);
    }

    static SyntaxStruct make(String[] names, SyntaxValue[] values,
                             String[] anns)
    {
        ImmutableStruct struct = immutableStruct(names, values, anns);
        return new SyntaxStruct(struct, null, null);
    }


    @Override
    boolean isNullValue()
    {
        return myStruct.isAnyNull();
    }


    @Override
    boolean hasNoChildren()
    {
        return myStruct.size() == 0;
    }


    @Override
    SyntaxStruct copyReplacingWraps(SyntaxWraps wraps)
    {
        // We can share the Map because its never mutated.
        return new SyntaxStruct(myStruct, getLocation(), wraps);
    }


    @Override
    SyntaxStruct stripWraps()
    {
        if (hasNoChildren()) return this;  // No children, no marks, all okay!

        // Even if we have no marks, some children may have them.
        boolean mustReplace = (myWraps != null);  // TODO optimize further

        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Map<String, Object> newMap =
            ((NonNullImmutableStruct) myStruct).copyMap();

        for (Map.Entry<String, Object> entry : newMap.entrySet())
        {
            Object value = entry.getValue();
            if (! (value instanceof Object[]))
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
                Object[] children = (Object[]) value;
                int childCount = children.length;

                boolean mustReplaceArray = false;
                Object[] newChildren = new Object[childCount];
                for (int i = 0; i < childCount; i++)
                {
                    SyntaxValue child = (SyntaxValue) children[i];
                    SyntaxValue stripped = child.stripWraps();
                    if (stripped != child)
                    {
                        mustReplaceArray = true;
                    }
                    newChildren[i] = stripped;
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

        ImmutableStruct struct;
        if (source.isNullValue())
        {
            struct = nullStruct(null /* FIXME eval*/, anns);
        }
        else
        {
            Map<String, Object> map = new HashMap<String, Object>();
            source.stepIn();
            while (source.next() != null)
            {
                String field = source.getFieldName();
                SyntaxValue child = Syntax.read(source, name);
                structImplAdd(map, field, child);
            }
            source.stepOut();

            struct = immutableStruct(map, anns);
        }

        return new SyntaxStruct(struct, loc, null);
    }


    @Override
    Type getType()
    {
        return Type.STRUCT;
    }


    SyntaxValue get(Evaluator eval, String fieldName)
        throws FusionException
    {
        // This should only be called at runtime, after wraps are pushed.
        assert myWraps == null;

        return (SyntaxValue) myStruct.dot(eval, fieldName);
    }


    @Override
    Object quote(Evaluator eval)  // TODO optimize
        throws FusionException
    {
        // This should only be called at runtime, after wraps are pushed.
        assert myWraps == null;

        if (myStruct.size() == 0)
        {
            return myStruct;
        }


        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Map<String, Object> newMap =
            ((NonNullImmutableStruct) myStruct).copyMap();

        for (Map.Entry<String, Object> entry : newMap.entrySet())
        {
            Object value = entry.getValue();
            if (! (value instanceof Object[]))
            {
                SyntaxValue child = (SyntaxValue) value;
                Object childValue = child.quote(eval);
                entry.setValue(childValue);
            }
            else
            {
                Object[] children = (Object[]) value;
                Object[] childValues = new Object[children.length];

                int cPos = 0;
                for (Object child : children)
                {
                    childValues[cPos++] = ((SyntaxValue)child).quote(eval);
                }
                entry.setValue(childValues);
            }
        }

        return immutableStruct(newMap, getAnnotations());
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env)
        throws FusionException
    {
        if (myStruct.size() == 0)
        {
            return this;
        }

        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Map<String, Object> newMap =
            ((NonNullImmutableStruct) myStruct).copyMap();

        for (Map.Entry<String, Object> entry : newMap.entrySet())
        {
            Object value = entry.getValue();
            if (! (value instanceof Object[]))
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
                Object[] children = (Object[]) value;
                int childCount = children.length;

                Object[] newChildren = new Object[childCount];
                for (int i = 0; i < childCount; i++)
                {
                    SyntaxValue subform = (SyntaxValue) children[i];
                    if (myWraps != null)
                    {
                        subform = subform.addWraps(myWraps);
                    }
                    newChildren[i] = subform.expand(eval, env);
                }
                entry.setValue(newChildren);
            }
        }


        // Wraps have been pushed down so the copy doesn't need them.
        return new SyntaxStruct(newMap, getAnnotations(), getLocation(), null);
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, FusionException
    {
        dispatchIonize(null, writer, myStruct);
    }


    //========================================================================


    @Override
    CompiledForm doCompile(final Evaluator eval, final Environment env)
        throws FusionException
    {
        assert myWraps == null;

        if (isNullValue())
        {
            return new CompiledConstant(NULL_STRUCT);
        }

        int size = myStruct.size();
        if (size == 0)
        {
            return new CompiledConstant(EMPTY_STRUCT);
        }

        final String[]       fieldNames = new String[size];
        final CompiledForm[] fieldForms = new CompiledForm[size];

        StructFieldVisitor visitor = new StructFieldVisitor()
        {
            int i = 0;

            @Override
            public Object visit(String name, Object value)
                throws FusionException
            {
                SyntaxValue child = (SyntaxValue) value;
                CompiledForm form = eval.compile(env, child);

                fieldNames[i] = name;
                fieldForms[i] = form;
                i++;
                return null;
            }
        };
        myStruct.visitFields(visitor);

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
