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

final class SyntaxList
    extends SyntaxSequence
{
    private SyntaxList(String[] anns, SourceLocation loc)
    {
        super(anns, loc);
    }

    static SyntaxList read(IonReader source, String[] anns)
    {
        SourceLocation loc = currentLocation(source);
        SyntaxList seq = new SyntaxList(anns, loc);
        seq.readChildren(source);
        return seq;
    }

    static SyntaxList make(SyntaxValue... children)
    {
        SyntaxList seq = new SyntaxList(EMPTY_STRING_ARRAY, null);
        seq.add(children);
        return seq;
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
    SyntaxValue prepare(Evaluator eval, Environment env)
        throws SyntaxFailure
    {
        int len = size();
        for (int i = 0; i < len; i++)
        {
            SyntaxValue subform = myChildren.get(i);
            SyntaxValue expanded = subform.prepare(eval, env);
            if (expanded != subform) myChildren.set(i, expanded);
        }
        return this;
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
