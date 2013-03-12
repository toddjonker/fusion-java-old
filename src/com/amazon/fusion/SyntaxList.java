// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.NULL_LIST;
import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionList.nullList;
import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static java.lang.System.arraycopy;
import static java.util.Arrays.copyOfRange;
import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxList
    extends SyntaxSequence
{
    /**
     * Both the array and its content may be shared with other instances.
     * When we push down wraps, we copy the array and the children.
     * We push lazily to aggregate as many wraps here and only push once.
     * That avoids repeated cloning of the children.
     */
    private SyntaxValue[] myChildren;


    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param anns must not be null.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param children the children of the new list.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    private SyntaxList(SourceLocation loc, String[] anns,
                       SyntaxValue[] children)
    {
        super(loc, anns);
        myChildren = children;
    }

    /**
     * Copy constructor, shares the myChildren array and replaces wraps.
     * The array will be copied when wraps are pushed but not before.
     */
    private SyntaxList(SyntaxList that, SyntaxWraps wraps)
    {
        super(that.getAnnotations(), that.getLocation(), wraps);
        assert that.myChildren.length != 0 && wraps != null;
        myChildren = that.myChildren;
    }


    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param anns must not be null.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     * @param children the children of the new list.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxList make(SourceLocation loc, String[] anns,
                           SyntaxValue[] children)
    {
        return new SyntaxList(loc, anns, children);
    }


    /**
     * Instance will be {@link #isNullValue()} if children is null.

     * @param children the children of the new list.
     * This method takes ownership of the array; the array and its elements
     * must not be changed by calling code afterwards!
     */
    static SyntaxList make(SourceLocation loc, SyntaxValue... children)
    {
        return new SyntaxList(loc, EMPTY_STRING_ARRAY, children);
    }



    /**
     * If we have wraps cached here, push them down into fresh copies of all
     * children. This must be called before exposing any children outside of
     * this instance, so that it appears as if the wraps were pushed when they
     * were created.
     */
    private void pushWraps()
        throws FusionException
    {
        if (myWraps != null)  // We only have wraps when we have children.
        {
            boolean changed = false;
            int len = myChildren.length;
            SyntaxValue[] newChildren = new SyntaxValue[len];
            for (int i = 0; i < len; i++)
            {
                SyntaxValue child = myChildren[i];
                SyntaxValue wrapped = child.addWraps(myWraps);
                newChildren[i] = wrapped;
                changed |= wrapped != child;
            }

            if (changed) myChildren = newChildren; // Keep sharing when we can

            myWraps = null;
        }
    }


    @Override
    SyntaxSequence stripWraps(Evaluator eval)
        throws FusionException
    {
        if (hasNoChildren()) return this;  // No children, no marks, all okay!

        // Even if we have no marks, some children may have them.
        boolean mustCopy = (myWraps != null);

        int len = myChildren.length;
        SyntaxValue[] newChildren = new SyntaxValue[len];
        for (int i = 0; i < len; i++)
        {
            SyntaxValue child = myChildren[i];
            SyntaxValue stripped = child.stripWraps(eval);
            newChildren[i] = stripped;
            mustCopy |= stripped != child;
        }

        if (! mustCopy) return this;

        return new SyntaxList(getLocation(), getAnnotations(), newChildren);
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
    final boolean isNullValue()
    {
        return myChildren == null;
    }


    @Override
    final boolean hasNoChildren()
    {
        return myChildren == null || myChildren.length == 0;
    }


    @Override
    final int size()
    {
        return (myChildren == null ? 0 : myChildren.length);
    }


    @Override
    SyntaxValue[] extract(Evaluator eval)
        throws FusionException
    {
        if (myChildren == null) return null;

        pushWraps();

        int len = myChildren.length;
        SyntaxValue[] extracted = new SyntaxValue[len];
        arraycopy(myChildren, 0, extracted, 0, len);
        return extracted;
    }


    @Override
    SyntaxValue get(Evaluator eval, int index)
        throws FusionException
    {
        pushWraps();
        return myChildren[index];
    }


    @Override
    SyntaxSequence makeAppended(Evaluator eval, SyntaxSequence that)
        throws FusionException
    {
        int thisLength = this.size();
        int thatLength = that.size();
        int newLength  = thisLength + thatLength;

        SyntaxValue[] children;
        if (newLength == 0)
        {
            children = SyntaxValue.EMPTY_ARRAY;
        }
        else
        {
            children = new SyntaxValue[thisLength + thatLength];
            if (thisLength != 0)
            {
                this.pushWraps();
                arraycopy(this.myChildren, 0, children, 0, thisLength);
            }
            if (thatLength != 0)
            {
                SyntaxValue[] c = that.extract(eval);
                arraycopy(c, 0, children, thisLength, thatLength);
            }
        }

        return new SyntaxList(null, EMPTY_STRING_ARRAY, children);
    }


    @Override
    SyntaxSequence makeSubseq(Evaluator eval, int from)
        throws FusionException
    {
        pushWraps();
        SyntaxValue[] children =
            (myChildren == null
                 ? null
                 : copyOfRange(myChildren, from, myChildren.length));
        return new SyntaxList(null, EMPTY_STRING_ARRAY, children);
    }


    void ionizeSequence(Evaluator eval, IonWriter writer, IonType type)
        throws IOException, FusionException
    {
        ionizeAnnotations(writer);
        if (myChildren == null)
        {
            writer.writeNull(type);
        }
        else
        {
            writer.stepIn(type);
            for (SyntaxValue child : myChildren)
            {
                child.ionize(eval, writer);
            }
            writer.stepOut();
        }
    }

    @Override
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        int len = size();
        if (len == 0) return this;

        SyntaxValue[] children = extract(expander.getEvaluator());

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
            SyntaxValue[] stxChildren = myChildren;

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
            pushWraps();
            children = myChildren;
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
            SyntaxValue elementExpr = get(eval, i);
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
