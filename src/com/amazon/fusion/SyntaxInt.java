// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;
import com.amazon.fusion.FusionNumber.BaseInt;
import java.math.BigInteger;

final class SyntaxInt
    extends SimpleSyntaxValue
{
    /**
     * @param datum must not be null.
     */
    private SyntaxInt(SourceLocation loc, BaseInt datum)
    {
        super(loc, datum);
    }


    /**
     * @param datum must be a Fusion int.
     */
    static SyntaxInt make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxInt(loc, (BaseInt) datum);
    }


    static SyntaxInt make(Evaluator      eval,
                          SourceLocation loc,
                          String[]       annotations,
                          BigInteger     value)
    {
        BaseInt datum = makeInt(eval, annotations, value);
        return new SyntaxInt(loc, datum);
    }


    static SyntaxInt make(Evaluator eval, int value)
    {
        BaseInt datum = makeInt(eval, value);
        return new SyntaxInt(/*location*/ null, datum);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.INT;
    }
}
