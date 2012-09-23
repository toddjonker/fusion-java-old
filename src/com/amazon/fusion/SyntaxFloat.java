// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
    Object quote(Evaluator eval)
    {
        return eval.newFloat(myValue, getAnnotations());
    }


    @Override
    void writeContentTo(IonWriter writer) throws IOException
    {
        writer.writeFloat(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
