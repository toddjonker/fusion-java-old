// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNull.makeNullNull;
import com.amazon.fusion.FusionNull.NullNull;

final class SyntaxNull
    extends SimpleSyntaxValue
{
    private SyntaxNull(SourceLocation loc, NullNull datum)
    {
        super(loc, datum);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     */
    static SyntaxNull make(Evaluator eval,
                           SourceLocation loc,
                           String[] annotations)
    {
        NullNull n = makeNullNull(eval, annotations);
        return new SyntaxNull(loc, n);
    }


    /**
     * @param datum must be a Fusion null.null.
     */
    static SyntaxNull make(Evaluator eval, SourceLocation loc, Object datum)
    {
        NullNull n = (NullNull) datum;
        return new SyntaxNull(loc, n);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.NULL;
    }
}
