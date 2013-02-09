// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;
import com.amazon.ion.IonWriter;
import java.io.IOException;
import java.math.BigInteger;

final class SyntaxInt
    extends SyntaxValue
{
    private final BigInteger myValue;


    private SyntaxInt(BigInteger value, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
        myValue = value;
    }

    static SyntaxInt make(BigInteger value, String[] anns, SourceLocation loc)
    {
        return new SyntaxInt(value, anns, loc);
    }

    static SyntaxInt make(int value)
    {
        return new SyntaxInt(BigInteger.valueOf(value),
                             EMPTY_STRING_ARRAY, null);
    }


    @Override
    Type getType()
    {
        return Type.INT;
    }


    @Override
    Object doCompileIonConstant(Evaluator eval, Environment env)
        throws FusionException
    {
        return eval.newInt(myValue);
    }


    @Override
    Object unwrap(Evaluator eval, boolean recurse)
    {
        return eval.newInt(myValue, getAnnotations());
    }


    @Override
    void ionize(Evaluator eval, IonWriter writer) throws IOException
    {
        ionizeAnnotations(writer);
        writer.writeInt(myValue);
    }


    @Override
    boolean isNullValue()
    {
        return myValue == null;
    }

    BigInteger bigIntegerValue()
    {
        return myValue;
    }
}
