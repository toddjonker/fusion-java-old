// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigInteger;

final class SyntaxInt
    extends SyntaxValue
{
    private final BigInteger myValue;


    private SyntaxInt(BigInteger value, SourceLocation loc)
    {
        super(loc);
        myValue = value;
    }

    static SyntaxInt make(BigInteger value, SourceLocation loc)
    {
        return new SyntaxInt(value, loc);
    }


    @Override
    Type getType()
    {
        return Type.INT;
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newInt(myValue);
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newInt(myValue);
    }


    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        writer.writeInt(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
