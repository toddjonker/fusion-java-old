// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Returns the next fusion value in the stream
 */
final class StreamNextProc
    extends Procedure
{
    StreamNextProc()
    {
        super("Returns the next fusion value in the STREAM.",
              "stream");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        Stream source = checkStreamArg(0,args);
        if (source.hasNext())
        {
            return source.next();
        }
        throw contractFailure("No new items to fetch from stream");
    }
}
