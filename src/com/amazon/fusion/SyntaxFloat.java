// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonType;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxFloat
    extends SyntaxValue
{
    private final Double myValue;


    private SyntaxFloat(Double value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxFloat make(Double value, String[] anns, SourceLocation loc)
    {
        return new SyntaxFloat(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.FLOAT;
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newFloat(myValue);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newFloat(myValue, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer) throws IOException
    {
        ionizeAnnotations(writer);
        if (myValue != null)
        {
            writer.writeFloat(myValue);
        }
        else
        {
            writer.writeNull(IonType.FLOAT);
        }
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
