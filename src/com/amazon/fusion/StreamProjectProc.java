// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Projects a stream onto another stream w/ use of transformation
 */
final class StreamProjectProc
    extends Procedure
{
    StreamProjectProc()
    {
        super("Projects a stream onto another stream w/ use of transformation",
              "stream", "proc");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        Stream source = checkStreamArg(0,args);
        Procedure proc = checkProcArg(1,args);
        return new ProjectStream(eval,proc,source);
    }
}
