// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.makeString;
import com.amazon.fusion.FusionString.BaseString;

final class SyntaxString
    extends SyntaxText
{
    /**
     * @param datum must not be null.
     */
    private SyntaxString(Evaluator eval, SourceLocation loc, BaseString datum)
    {
        super(loc, datum);
    }


    /**
     * @param datum must not be null.
     */
    static SyntaxString make(Evaluator eval,
                             SourceLocation loc,
                             BaseString datum)
    {
        return new SyntaxString(eval, loc, datum);
    }


    /**
     * @param datum must be a Fusion string.
     */
    static SyntaxString make(Evaluator eval, SourceLocation loc, Object datum)
    {
        BaseString string = (BaseString) datum;
        return new SyntaxString(eval, loc, string);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    static SyntaxString make(Evaluator eval,
                             SourceLocation loc,
                             String[] annotations,
                             String value)
    {
        BaseString datum = makeString(eval, annotations, value);
        return new SyntaxString(eval, loc, datum);
    }


    /**
     * @param value may be null.
     */
    static SyntaxString make(Evaluator eval, String value)
    {
        BaseString datum = makeString(eval, value);
        return new SyntaxString(eval, null, datum);
    }
}
