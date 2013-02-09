// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import com.amazon.ion.Timestamp;
import java.io.IOException;

final class SyntaxTimestamp
    extends SyntaxValue
{
    private final Timestamp myValue;


    private SyntaxTimestamp(Timestamp value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxTimestamp make(Timestamp value, String[] anns,
                                SourceLocation loc)
    {
        return new SyntaxTimestamp(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.TIMESTAMP;
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newTimestamp(myValue);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newTimestamp(myValue, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer) throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeTimestamp(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
