// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigDecimal;

final class SyntaxDecimal
    extends SyntaxValue
{
    private final BigDecimal myValue;


    private SyntaxDecimal(BigDecimal value, SourceLocation loc)
    {
        super(loc);
        myValue = value;
    }

    static SyntaxDecimal make(BigDecimal value, SourceLocation loc)
    {
        return new SyntaxDecimal(value, loc);
    }


    @Override
    Type getType()
    {
        return Type.DECIMAL;
    }


    @Override
    FusionValue eval(Evaluator eval, Environment env) throws FusionException
    {
        return eval.newDecimal(myValue);
    }


    @Override
    FusionValue quote(Evaluator eval)
    {
        return eval.newDecimal(myValue);
    }


    @Override
    void writeTo(IonWriter writer) throws IOException
    {
        writer.writeDecimal(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
