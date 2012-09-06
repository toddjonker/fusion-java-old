// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionUtils.cloneIfContained;

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
    SyntaxList makeSimilar(SyntaxValue[] children)
    {
        return make(null, children);
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        int len = size();
        if (len == 0) return this;

        SyntaxValue[] children = extract();

        for (int i = 0; i < len; i++)
        {
            SyntaxValue subform = children[i];
            children[i] = subform.prepare(eval, env);
        }

        SyntaxList expanded = SyntaxList.make(this.getLocation(), children);
        return expanded;
    }


    @Override
    public FusionValue eval(Evaluator eval, Environment env)
        throws FusionException
    {
        ValueFactory vf = eval.getSystem();
        IonList resultDom;
        if (isNullValue())
        {
            resultDom = vf.newNullList();
        }
        else
        {
            resultDom = vf.newEmptyList();
            int len = size();
            for (int i = 0; i < len; i++)
            {
                SyntaxValue elementExpr = get(i);
                FusionValue elementValue = eval.eval(env, elementExpr);
                IonValue elementDom = FusionValue.toIonValue(elementValue);
                elementDom = cloneIfContained(elementDom);
                resultDom.add(elementDom);
            }
        }
        return new DomValue(resultDom);
    }


    @Override
    void writeContentTo(IonWriter writer)
        throws IOException
    {
        writeContentTo(writer, IonType.LIST);
    }
}
