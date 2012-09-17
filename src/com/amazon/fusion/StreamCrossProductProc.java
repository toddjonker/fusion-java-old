// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Generates the stream to represent the cross product of 2 streams
 */
final class StreamCrossProductProc
    extends Procedure
{
    StreamCrossProductProc()
    {
        super("Generates the stream to represent the " +
              "cross product of 2 streams");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        Stream source1 = checkStreamArg(0,args);
        Stream source2 = checkStreamArg(1,args);
        return new CrossProductStream(eval,source1,source2);
    }
}
