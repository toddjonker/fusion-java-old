// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBlob.makeBlob;
import com.amazon.fusion.FusionBlob.BaseBlob;

final class SyntaxBlob
    extends SimpleSyntaxValue
{
    private SyntaxBlob(SourceLocation loc, BaseBlob datum)
    {
        super(loc, datum);
    }


    /**
     * @param datum must be a Fusion blob.
     */
    static SyntaxBlob make(Evaluator eval, SourceLocation loc, Object datum)
    {
        return new SyntaxBlob(loc, (BaseBlob) datum);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     * This method assumes ownership of the array and it must not be modified
     * later.
     */
    static SyntaxBlob make(Evaluator      eval,
                           SourceLocation loc,
                           String[]       annotations,
                           byte[]         value)
    {
        BaseBlob datum = makeBlob(eval, annotations, value);
        return new SyntaxBlob(loc, datum);
    }


    //========================================================================


    @Override
    Type getType()
    {
        return Type.BLOB;
    }
}
