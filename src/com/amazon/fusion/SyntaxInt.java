// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigInteger;

final class SyntaxInt
    extends SyntaxValue
{
    private final BigInteger myValue;


    private SyntaxInt(BigInteger value)
    {
        myValue = value;
    }

    static SyntaxInt read(IonReader source)
    {
        return new SyntaxInt(source.bigIntegerValue());
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
