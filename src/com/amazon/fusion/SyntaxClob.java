// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionClob.makeClob;
import com.amazon.fusion.FusionClob.BaseClob;

final class SyntaxClob
    extends SimpleSyntaxValue
{
    private SyntaxClob(SourceLocation loc, BaseClob datum)
    {
        super(loc, datum);
    }


    /**
     * @param datum must be a Fusion clob.
     */
    static SyntaxClob make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxClob(loc, (BaseClob) datum);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     * This method assumes ownership of the array and it must not be modified
     * later.
     */
    static SyntaxClob make(Evaluator      eval,
                           SourceLocation loc,
                           String[]       annotations,
                           byte[]         value)
    {
        BaseClob datum = makeClob(eval, annotations, value);
        return new SyntaxClob(loc, datum);
    }
}
