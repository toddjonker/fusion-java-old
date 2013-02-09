// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newBlob(myValue, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer) throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeBlob(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
