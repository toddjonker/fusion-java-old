// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionList.isImmutableList;
import static com.amazon.fusion.FusionList.unsafeListRef;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static java.lang.System.arraycopy;
import com.amazon.fusion.FusionList.BaseList;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxList
    extends SyntaxSequence
{
    /**
     * Both the list and its elements may be shared with other instances.
     * When we push down wraps, we copy the list and the children as needed.
     * We push lazily to aggregate as many wraps here and only push once.
     * That avoids repeated cloning of the children.
     */
    private BaseList myImmutableList;


    /**
     * @param datum an immutable list of {@link SyntaxValue}s.
     */
    private SyntaxList(Evaluator eval,
                       SourceLocation loc,
                       BaseList datum)
    {
        super(loc, datum.annotationsAsJavaStrings());
        assert isImmutableList(eval, datum);
        myImmutableList = datum;
    }

    /**
     * Copy constructor, shares the myChildren array and replaces wraps.
     * The array will be copied when wraps are pushed but not before.
     */
    private SyntaxList(SyntaxList that, SyntaxWraps wraps)
    {
        super(that.getAnnotations(), that.getLocation(), wraps);
        assert wraps != null;
        myImmutableList = that.myImmutableList;
    }



    /**
     * @param datum an immutable list of {@link SyntaxValue}s.
     */
    static SyntaxList make(Evaluator      eval,
                           SourceLocation loc,
                           Object         datum)
    {
        return new SyntaxList(eval, loc, (BaseList) datum);
    }



    /**
     * If we have wraps cached here, push them down into fresh copies of all
     * children. This must be called before exposing any children outside of
     * this instance, so that it appears as if the wraps were pushed when they
     * were created.
     */
    private synchronized void pushWraps(Evaluator eval)
        throws FusionException
    {
        if (myWraps != null)  // We only have wraps when we have children.
        {
            boolean changed = false;
            int len = myImmutableList.size();
            SyntaxValue[] newChildren = new SyntaxValue[len];
            for (int i = 0; i < len; i++)
            {
                SyntaxValue child = (SyntaxValue) myImmutableList.elt(eval, i);
                SyntaxValue wrapped = child.addWraps(myWraps);
                newChildren[i] = wrapped;
                changed |= wrapped != child;
            }

            if (changed) // Keep sharing when we can
            {
                myImmutableList =
                    immutableList(eval, getAnnotations(), newChildren);
            }

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

        int len = myImmutableList.size();
        SyntaxValue[] newChildren = new SyntaxValue[len];
        for (int i = 0; i < len; i++)
        {
            SyntaxValue child = (SyntaxValue) myImmutableList.elt(eval, i);
            SyntaxValue stripped = child.stripWraps(eval);
            newChildren[i] = stripped;
            mustCopy |= stripped != child;
        }

        if (! mustCopy) return this;

        BaseList newList =
            immutableList(eval,
                          myImmutableList.annotationsAsJavaStrings(),
                          newChildren);
        return new SyntaxList(eval, getLocation(), newList);
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
        return myImmutableList.isAnyNull();
    }


    @Override
    final boolean hasNoChildren()
    {
        return myImmutableList.size() == 0;
    }


    @Override
    final int size()
    {
        return myImmutableList.size();
    }


    @Override
    SyntaxValue[] extract(Evaluator eval)
        throws FusionException
    {
        if (isNullValue()) return null;

        pushWraps(eval);

        int len = myImmutableList.size();
        SyntaxValue[] extracted = new SyntaxValue[len];
        myImmutableList.unsafeCopy(eval, 0, extracted, 0, len);
        return extracted;
    }


    @Override
    SyntaxValue get(Evaluator eval, int index)
        throws FusionException
    {
        pushWraps(eval);
        return (SyntaxValue) myImmutableList.elt(eval, index);
    }


    @Override
    SyntaxSequence makeAppended(Evaluator eval, SyntaxSequence that)
        throws FusionException
    {
        int thisLength = this.size();
        int thatLength = that.size();
        int newLength  = thisLength + thatLength;

        if (newLength == 0) return this;

        Object[] children = new Object[newLength];
        if (thisLength != 0)
        {
            pushWraps(eval);
            myImmutableList.unsafeCopy(eval, 0, children, 0, thisLength);
        }
        if (thatLength != 0)
        {
            // that could be a sexp so we cant copy directly
            // TODO avoid intermediate array copy
            SyntaxValue[] c = that.extract(eval);
            arraycopy(c, 0, children, thisLength, thatLength);
        }

        BaseList list = immutableList(eval, getAnnotations(), children);
        return new SyntaxList(eval, null, list);
    }


    @Override
    SyntaxSequence makeSubseq(Evaluator eval, int from)
        throws FusionException
    {
        if ((myImmutableList.size() == 0 || from == 0)
            && myImmutableList.annotationsAsJavaStrings().length == 0)
        {
            return this;
        }

        pushWraps(eval);

        BaseList list;
        if (isNullValue())
        {
            list = FusionList.NULL_LIST;
        }
        else
        {
            // TODO will crash is from is beyond the end of the list
            int len = myImmutableList.size();
            Object[] children = new Object[len - from];
            myImmutableList.unsafeCopy(eval, from, children, 0, children.length);
            list = immutableList(eval, EMPTY_STRING_ARRAY, children);
        }

        return new SyntaxList(eval, null, list);
    }


    @Override
    SyntaxValue doExpand(Expander expander, Environment env)
        throws FusionException
    {
        int len = size();
        if (len == 0) return this;

        Evaluator eval = expander.getEvaluator();

        Object list = unwrap(eval, false);

        boolean same = true;
        Object[] children = new Object[len];
        for (int i = 0; i < len; i++)
        {
            SyntaxValue subform = (SyntaxValue) unsafeListRef(eval, list, i);
            Object expanded = expander.expandExpression(env, subform);
            same &= (subform == expanded);
            children[i] = expanded;
        }

        if (same) return this;

        list = immutableList(eval, getAnnotations(), children);
        return SyntaxList.make(eval, getLocation(), list);
    }


    @Override
    void ionize(Evaluator eval, IonWriter out)
        throws IOException, FusionException
    {
        myImmutableList.ionize(eval, out);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
        throws FusionException
    {
        int size = myImmutableList.size();
        if (size == 0)
        {
            return myImmutableList;
        }

        if (! recurse)
        {
            pushWraps(eval);
            return myImmutableList;
        }

        // Don't bother to push wraps; we'll just discard them anyway.
        Object[] children = new Object[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue child = (SyntaxValue) myImmutableList.elt(eval, i);
            children[i] = child.unwrap(eval, true);
        }

        String[] annotations = getAnnotations();
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
            return new CompiledConstant(myImmutableList);
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
