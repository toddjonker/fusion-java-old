// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.NULL_LIST;
import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionList.nullList;
import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxList
    extends SyntaxSequence
{
    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children the children of the new list.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param anns must not be null.
     */
    private SyntaxList(SyntaxValue[] children, String[] anns,
                       SourceLocation loc)
    {
        super(children, anns, loc);
    }

    /** Copy constructor shares children and replaces unpushed wraps. */
    private SyntaxList(SyntaxList that, SyntaxWraps wraps)
    {
        super(that, wraps);
    }


    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new list.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param anns must not be null.
     */
    static SyntaxList make(SyntaxValue[] children, String[] anns,
                           SourceLocation loc)
    {
        return new SyntaxList(children, anns, loc);
    }


    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new list.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxList make(SourceLocation loc, SyntaxValue... children)
    {
        return new SyntaxList(children, EMPTY_STRING_ARRAY, loc);
    }


    @Override
    SyntaxList copyReplacingWraps(SyntaxWraps wraps)
    {
        return new SyntaxList(this, wraps);
    }


    @Override
    Type getType()
    {
        return Type.LIST;
    }


    @Override
    SyntaxList makeSimilar(Evaluator eval,
                           SyntaxValue[] children,
                           String[] anns,
                           SourceLocation loc)
    {
        return new SyntaxList(children, anns, loc);
    }


    @Override
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        int len = size();
        if (len == 0) return this;

        SyntaxValue[] children = extract();

        for (int i = 0; i < len; i++)
        {
            SyntaxValue subform = children[i];
            children[i] = expander.expandExpression(env, subform);
        }

        SyntaxList expanded = SyntaxList.make(this.getLocation(), children);
        return expanded;
    }


    @Override
    void ionize(Evaluator eval, IonWriter out)
        throws IOException, FusionException
    {
        ionizeSequence(eval, out, IonType.LIST);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
        throws FusionException
    {
        String[] annotations = getAnnotations();

        if (isNullValue())
        {
            return nullList(eval, annotations);
        }

        Object[] children;

        if (recurse)
        {
            // Don't bother to push wraps; we'll just discard them anyway.
            SyntaxValue[] stxChildren = children();

            int size = stxChildren.length;
            if (size == 0)  // Avoid allocation of child array
            {
                children = EMPTY_OBJECT_ARRAY;
            }
            else
            {
                children = new Object[size];
                for (int i = 0; i < size; i++)
                {
                    children[i] = stxChildren[i].unwrap(eval, true);
                }
            }
        }
        else
        {
            children = unwrapChildren();
        }

        return immutableList(eval, annotations, children);
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        // We don't have to worry about annotations, since that's not valid
        // syntax.
        if (isNullValue())
        {
            return new CompiledConstant(NULL_LIST);
        }

        int len = size();
        CompiledForm[] children = new CompiledForm[len];
        for (int i = 0; i < len; i++)
        {
            SyntaxValue elementExpr = get(i);
            CompiledForm child = eval.compile(env, elementExpr);
            children[i] = child;
        }
        return new CompiledList(children);

    }


    //========================================================================


    private static final class CompiledList
        implements CompiledForm
    {
        private final CompiledForm[] myChildForms;

        CompiledList(CompiledForm[] childForms)
        {
            myChildForms = childForms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            int len = myChildForms.length;
            Object[] children = new Object[len];
            for (int i = 0; i < len; i++)
            {
                children[i] = eval.eval(store, myChildForms[i]);
            }

            return immutableList(eval, children);
        }
    }
}
