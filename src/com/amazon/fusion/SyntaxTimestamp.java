// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionTimestamp.makeTimestamp;
import com.amazon.fusion.FusionTimestamp.BaseTimestamp;
import com.amazon.ion.Timestamp;

final class SyntaxTimestamp
    extends SimpleSyntaxValue
{
    private SyntaxTimestamp(SourceLocation loc, BaseTimestamp datum)
    {
        super(loc, datum);
    }


    /**
     * @param fusionTimestamp must be a Fusion timestamp.
     */
    static SyntaxTimestamp make(Evaluator      eval,
                                SourceLocation loc,
                                Object         fusionTimestamp)
    {
        BaseTimestamp ts = (BaseTimestamp) fusionTimestamp;
        return new SyntaxTimestamp(loc, ts);
    }


    /**
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
     * @param value may be null.
     */
    static SyntaxTimestamp make(Evaluator      eval,
                                SourceLocation loc,
                                String[]       annotations,
                                Timestamp      value)
    {
        BaseTimestamp ts = makeTimestamp(eval, annotations, value);
        return new SyntaxTimestamp(loc, ts);
    }
}
