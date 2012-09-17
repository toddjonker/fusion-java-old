// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Selects elements from a stream if they satisfy some
 * user-defined conditional procedure
 */
final class StreamSelectProc
    extends Procedure
{
    StreamSelectProc()
    {
        super("Selects elements from a STREAM if they satisfy the PREDICATE.",
              "stream", "predicate");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        Stream source = checkStreamArg(0,args);
        Procedure proc = checkProcArg(1,args);
        return new SelectStream(eval,proc,source);
    }
}
