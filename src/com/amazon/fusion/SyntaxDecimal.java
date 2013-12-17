// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeDecimal;
import com.amazon.fusion.FusionNumber.BaseDecimal;
import java.math.BigDecimal;

final class SyntaxDecimal
    extends SimpleSyntaxValue
{
    /**
     * @param datum must not be null.
     */
    private SyntaxDecimal(SourceLocation loc, BaseDecimal datum)
    {
        super(loc, datum);
    }


    /**
     * @param datum must be a Fusion decimal.
     */
    static SyntaxDecimal make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxDecimal(loc, (BaseDecimal) datum);
    }

    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    static SyntaxDecimal make(Evaluator      eval,
                              SourceLocation loc,
                              String[]       annotations,
                              BigDecimal     value)
    {
        BaseDecimal datum = makeDecimal(eval, annotations, value);
        return new SyntaxDecimal(loc, datum);
    }
}
