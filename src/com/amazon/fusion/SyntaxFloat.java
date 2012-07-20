// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import com.amazon.ion.IonWriter;
import java.io.IOException;

final class SyntaxFloat
    extends SyntaxValue
{
    private final Double myValue;


    private SyntaxFloat(Double value)
    {
        myValue = value;
    }

    static SyntaxFloat read(IonReader source)
    {
        Double value =
            (source.isNullValue() ? null : source.doubleValue());
        return new SyntaxFloat(value);
    }


    @Override
    Type getType()
    {
        return Type.FLOAT;
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newFloat(myValue);
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newFloat(myValue);
    }


    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        writer.writeFloat(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
