// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxClob
    extends SyntaxValue
{
    private final byte[] myValue;


    private SyntaxClob(byte[] value)
    {
        myValue = value;
    }

    static SyntaxClob read(IonReader source)
    {
        byte[] value = (source.isNullValue() ? null : source.newBytes());
        return new SyntaxClob(value);
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
