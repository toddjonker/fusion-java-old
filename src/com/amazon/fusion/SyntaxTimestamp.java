// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonWriter;
import com.amazon.ion.Timestamp;
import java.io.IOException;

final class SyntaxTimestamp
    extends SyntaxValue
{
    private final Timestamp myValue;


    private SyntaxTimestamp(Timestamp value)
    {
        myValue = value;
    }

    static SyntaxTimestamp read(IonReader source)
    {
        Timestamp value = source.timestampValue();
        return new SyntaxTimestamp(value);
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
