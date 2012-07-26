// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxBlob
    extends SyntaxValue
{
    private final byte[] myValue;


    private SyntaxBlob(byte[] value, SourceLocation loc)
    {
        super(loc);
        myValue = value;
    }

    static SyntaxBlob make(byte[] value, SourceLocation loc)
    {
        return new SyntaxBlob(value, loc);
    }


    @Override
    Type getType()
    {
        return Type.BLOB;
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newBlob(myValue);
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newBlob(myValue);
    }


    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        writer.writeBlob(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
