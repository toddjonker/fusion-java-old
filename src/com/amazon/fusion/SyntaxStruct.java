// Copyright (c) 2012-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.immutableStruct;
import com.amazon.fusion.FusionStruct.ImmutableStruct;
import com.amazon.fusion.FusionStruct.NonNullImmutableStruct;
import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.ion.IonException;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.util.Map;

final class SyntaxStruct
    extends SyntaxContainer
{
    private ImmutableStruct myStruct;


    /**
     * @param struct must not be null.
     */
    private SyntaxStruct(SourceLocation  loc,
                         Object[] properties,
                         SyntaxWraps wraps,
                         ImmutableStruct struct)
    {
        super(loc, properties, wraps);
        myStruct = struct;
    }

    /**
     * @param struct must not be null.
     */
    private SyntaxStruct(SourceLocation  loc,
                         ImmutableStruct struct)
    {
        super(loc);
        myStruct = struct;
    }



    static SyntaxStruct makeOriginal(Evaluator       eval,
                                     SourceLocation  loc,
                                     ImmutableStruct struct)
    {
        return new SyntaxStruct(loc, ORIGINAL_STX_PROPS, null, struct);
    }


    /**
     * @param datum must be an immutable struct
     */
    static SyntaxStruct make(Evaluator eval,
                             SourceLocation loc,
                             Object datum)
    {
        return new SyntaxStruct(loc, (ImmutableStruct) datum);
    }


    //========================================================================


    @Override
    Object visit(Visitor v) throws FusionException
    {
        return v.accept(this);
    }


    @Override
    boolean hasNoChildren()
    {
        return myStruct.size() == 0;
    }


    @Override
    SyntaxStruct copyReplacingProperties(Object[] properties)
    {
        return new SyntaxStruct(getLocation(), properties, myWraps, myStruct);
    }

    @Override
    SyntaxStruct copyReplacingWraps(SyntaxWraps wraps)
    {
        return new SyntaxStruct(getLocation(), getProperties(), wraps,
                                myStruct);
    }


    @Override
    SyntaxStruct stripWraps(Evaluator eval)
        throws FusionException
    {
        if (hasNoChildren()) return this;  // No children, no marks, all okay!

        // Even if we have no marks, some children may have them.
        boolean mustReplace = (myWraps != null);  // TODO optimize further

        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Map<String, Object> newMap =
            ((NonNullImmutableStruct) myStruct).copyMap(eval);

        for (Map.Entry<String, Object> entry : newMap.entrySet())
        {
            Object value = entry.getValue();
            if (! (value instanceof Object[]))
            {
                SyntaxValue child = (SyntaxValue) value;
                SyntaxValue stripped = child.stripWraps(eval);
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
                    SyntaxValue stripped = child.stripWraps(eval);
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

        BaseSymbol[] annotations = myStruct.getAnnotations();
        ImmutableStruct s = immutableStruct(newMap, annotations);
        return new SyntaxStruct(getLocation(), getProperties(), null, s);
    }


    SyntaxValue get(Evaluator eval, String fieldName)
        throws FusionException
    {
        // This should only be called at runtime, after wraps are pushed.
        assert myWraps == null;

        return (SyntaxValue) myStruct.elt(eval, fieldName);
    }


    @Override
    Object unwrap(Evaluator eval)
        throws FusionException
    {
        if (myWraps == null)
        {
            return myStruct;
        }

        // We have wraps to propagate (and therefore children).

        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Map<String, Object> newMap =
            ((NonNullImmutableStruct) myStruct).copyMap(eval);

        // TODO optimize this to not allocate new objects when nothing changes.
        // Idea: keep track of when there are symbols contained (recursively),
        // when there's not, maybe we can skip all this.

        for (Map.Entry<String, Object> entry : newMap.entrySet())
        {
            Object value = entry.getValue();
            if (! (value instanceof Object[]))
            {
                SyntaxValue child = (SyntaxValue) value;
                Object childValue = child.addWraps(myWraps);
                entry.setValue(childValue);
            }
            else
            {
                Object[] children = (Object[]) value;
                Object[] childValues = new Object[children.length];

                int cPos = 0;
                for (Object c : children)
                {
                    SyntaxValue child = (SyntaxValue) c;
                    Object childValue = child.addWraps(myWraps);
                    childValues[cPos++] = childValue;
                }
                entry.setValue(childValues);
            }
        }

        BaseSymbol[] annotations = myStruct.getAnnotations();
        myStruct = immutableStruct(newMap, annotations);
        myWraps = null;

        return myStruct;
    }


    @Override
    Object syntaxToDatum(Evaluator eval)
        throws FusionException
    {
        if (myStruct.size() == 0)
        {
            return myStruct;
        }

        // We have children, and wraps to propagate (when not recursing)

        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Map<String, Object> newMap =
            ((NonNullImmutableStruct) myStruct).copyMap(eval);

        // TODO optimize this to not allocate new objects when nothing changes.

        for (Map.Entry<String, Object> entry : newMap.entrySet())
        {
            Object value = entry.getValue();
            if (! (value instanceof Object[]))
            {
                SyntaxValue child = (SyntaxValue) value;
                Object childValue = child.syntaxToDatum(eval);
                entry.setValue(childValue);
            }
            else
            {
                Object[] children = (Object[]) value;
                Object[] childValues = new Object[children.length];

                int cPos = 0;
                for (Object c : children)
                {
                    SyntaxValue child = (SyntaxValue) c;
                    Object childValue = child.syntaxToDatum(eval);
                    childValues[cPos++] = childValue;
                }
                entry.setValue(childValues);
            }
        }

        BaseSymbol[] annotations = myStruct.getAnnotations();
        return immutableStruct(newMap, annotations);
    }


    @Override
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        if (myStruct.size() == 0)
        {
            return this;
        }

        // Make a copy of the map, then mutate it to replace children
        // as necessary.
        Evaluator eval = expander.getEvaluator();
        Map<String, Object> newMap =
            ((NonNullImmutableStruct) myStruct).copyMap(eval);

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
                subform = expander.expandExpression(env, subform);
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
                    newChildren[i] = expander.expandExpression(env, subform);
                }
                entry.setValue(newChildren);
            }
        }


        // Wraps have been pushed down so the copy doesn't need them.
        BaseSymbol[] annotations = myStruct.getAnnotations();
        ImmutableStruct s = immutableStruct(newMap, annotations);
        return new SyntaxStruct(getLocation(), s);
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer)
        throws IOException, IonException, FusionException, IonizeFailure
    {
        myStruct.ionize(eval, writer);
    }

    @Override
    final void write(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        myStruct.write(eval, out);
    }
}
