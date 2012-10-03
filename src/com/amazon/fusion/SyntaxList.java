// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionVector.isVector;
import com.amazon.ion.IonList;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
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
    IonSequence makeNull(ValueFactory factory)
    {
        return factory.newNullList();
    }


    @Override
    SyntaxList makeSimilar(SyntaxValue[] children, String[] anns,
                           SourceLocation loc)
    {
        return new SyntaxList(children, anns, loc);
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        int len = size();
        if (len == 0) return this;

        SyntaxValue[] children = extract();

        for (int i = 0; i < len; i++)
        {
            SyntaxValue subform = children[i];
            children[i] = subform.expand(eval, env);
        }

        SyntaxList expanded = SyntaxList.make(this.getLocation(), children);
        return expanded;
    }


    @Override
    void writeContentTo(IonWriter writer)
        throws IOException
    {
        writeContentTo(writer, IonType.LIST);
    }


    @Override
    Object quote(Evaluator eval)
        throws FusionException
    {
        if (getAnnotations().length != 0)
        {
            return super.quote(eval);
        }

        if (isNullValue())
        {

            ValueFactory factory = eval.getSystem();
            IonSequence seq = makeNull(factory);
            return eval.inject(seq);
        }

        int size = size();
        if (size == 0)
        {
            return FusionVector.EMPTY_IMMUTABLE_VECTOR;
        }

        Object[] children = new Object[size];
        for (int i = 0; i < size; i++)
        {
            SyntaxValue s = get(i);
            children[i] = s.quote(eval);
        }

        return FusionVector.makeImmutableVectorFrom(eval, children);
    }


    //========================================================================


    @Override
    CompiledForm doCompile(Evaluator eval, Environment env)
        throws FusionException
    {
        ValueFactory vf = eval.getSystem();
        if (isNullValue())
        {
            IonList list = vf.newNullList();
            return new CompiledIonConstant(list);
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
                Object childValue = eval.eval(store, myChildForms[i]);
                if (! isVector(eval, childValue))
                {
                    IonValue childDom = FusionValue.castToIonValueMaybe(childValue);
                    if (childDom == null)
                    {
                        throw new ResultFailure("list literal", "Ion data", i,
                                                childValue);
                    }
                }

                children[i] = childValue;
            }

            return FusionVector.makeImmutableVectorFrom(eval, children);
        }
    }
}
