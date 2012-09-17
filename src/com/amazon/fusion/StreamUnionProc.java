// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Takes the union of 2 streams
 */
final class StreamUnionProc
    extends Procedure
{
    // TODO list of streams / union of multiple streams

    StreamUnionProc()
    {
        super("Takes the union of 2 streams");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(2, args);
        Stream source1 = checkStreamArg(0,args);
        Stream source2 = checkStreamArg(1,args);

        return new UnionStream(source1,source2);
    }
}
