// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxClob
    extends SyntaxValue
{
    private final byte[] myValue;


    private SyntaxClob(byte[] value, SourceLocation loc)
    {
        super(loc);
        myValue = value;
    }

    static SyntaxClob make(byte[] value, SourceLocation loc)
    {
        return new SyntaxClob(value, loc);
    }


    @Override
    Type getType()
    {
        return Type.CLOB;
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newClob(myValue);
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newClob(myValue);
    }


    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        writer.writeClob(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
