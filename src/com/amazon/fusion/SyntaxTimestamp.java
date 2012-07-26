// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import com.amazon.ion.Timestamp;
import java.io.IOException;

final class SyntaxTimestamp
    extends SyntaxValue
{
    private final Timestamp myValue;


    private SyntaxTimestamp(Timestamp value, SourceLocation loc)
    {
        super(loc);
        myValue = value;
    }

    static SyntaxTimestamp make(Timestamp value, SourceLocation loc)
    {
        return new SyntaxTimestamp(value, loc);
    }


    @Override
    Type getType()
    {
        return Type.TIMESTAMP;
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newTimestamp(myValue);
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newTimestamp(myValue);
    }


    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        writer.writeTimestamp(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
