// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class StreamHasNextProc
    extends Procedure
{
    StreamHasNextProc()
    {
        super("Returns whether there are values left to fetch in the STREAM.",
              "stream");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        Stream source = checkStreamArg(0, args);

        boolean hasNext = source.hasNext();
        return eval.newBool(hasNext);
    }
}
