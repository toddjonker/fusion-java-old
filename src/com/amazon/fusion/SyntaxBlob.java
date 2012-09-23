// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxBlob
    extends SyntaxValue
{
    private final byte[] myValue;


    private SyntaxBlob(byte[] value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxBlob make(byte[] value, String[] anns, SourceLocation loc)
    {
        return new SyntaxBlob(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.BLOB;
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newBlob(myValue);
    }


    @Override
    Object quote(Evaluator eval)
    {
        return eval.newBlob(myValue, getAnnotations());
    }


    @Override
    void writeContentTo(IonWriter writer) throws IOException
    {
        writer.writeBlob(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
