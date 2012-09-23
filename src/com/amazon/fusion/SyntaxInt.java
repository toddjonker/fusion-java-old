// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigInteger;

final class SyntaxInt
    extends SyntaxValue
{
    private final BigInteger myValue;


    private SyntaxInt(BigInteger value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxInt make(BigInteger value, String[] anns, SourceLocation loc)
    {
        return new SyntaxInt(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.INT;
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newInt(myValue);
    }


    @Override
    Object quote(Evaluator eval)
    {
        return eval.newInt(myValue, getAnnotations());
    }


    @Override
    void writeContentTo(IonWriter writer) throws IOException
    {
        writer.writeInt(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }

    BigInteger bigIntegerValue()
    {
        return myValue;
    }
}
