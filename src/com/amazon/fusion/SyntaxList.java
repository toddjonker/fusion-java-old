// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import static com.amazon.fusion.FusionUtils.cloneIfContained;
import static com.amazon.fusion.SourceLocation.currentLocation;
import com.amazon.ion.IonList;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.ValueFactory;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

final class SyntaxList
    extends SyntaxSequence
{
    private SyntaxList(String[] anns, SourceLocation loc)
    {
        super(anns, loc);
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children this instance takes ownership and it must not be modified!
     */
    private SyntaxList(String[] anns, SourceLocation loc,
                       List<SyntaxValue> children)
    {
        super(anns, loc, children);
    }


    static SyntaxList read(IonReader source, String[] anns)
    {
        SourceLocation loc = currentLocation(source);
        SyntaxList seq = new SyntaxList(anns, loc, readChildren(source));
        return seq;
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     */
    static SyntaxList make(SourceLocation loc, SyntaxValue... children)
    {
        SyntaxList seq;
        if (children == null)
        {
            seq = new SyntaxList(EMPTY_STRING_ARRAY, loc);
            assert seq.isNullValue();
        }
        else
        {
            List<SyntaxValue> childs = Arrays.asList(children);
            seq = new SyntaxList(EMPTY_STRING_ARRAY, loc, childs);
        }

        return seq;
    }

    /**
     * Instance will be {@link #isNullValue()} if children is null.
     *
     * @param children this instance takes ownership and it must not be modified!
     */
    static SyntaxList make(SourceLocation loc, List<SyntaxValue> children)
    {
        return new SyntaxList(EMPTY_STRING_ARRAY, loc, children);
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
    SyntaxList makeSimilar(List<SyntaxValue> children)
    {
        return make(null, children);
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        pushAnyWraps();          // Wraps go away if we bail out on empty list
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
