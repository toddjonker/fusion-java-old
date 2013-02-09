// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigDecimal;

final class SyntaxDecimal
    extends SyntaxValue
{
    private final BigDecimal myValue;


    private SyntaxDecimal(BigDecimal value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxDecimal make(BigDecimal value, String[] anns,
                              SourceLocation loc)
    {
        return new SyntaxDecimal(value, anns, loc);
    }


    @Override
    Type getType()
    {
        return Type.DECIMAL;
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newDecimal(myValue);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newDecimal(myValue, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer) throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeDecimal(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }
}
