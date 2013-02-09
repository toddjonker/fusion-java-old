// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxClob
    extends SyntaxValue
{
    private final byte[] myValue;


    private SyntaxClob(byte[] value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxClob make(byte[] value, String[] anns, SourceLocation loc)
    {
        return new SyntaxClob(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.CLOB;
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newClob(myValue);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newClob(myValue, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer) throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeClob(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
