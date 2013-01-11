// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.Timestamp;

/*
 * Generates the current timestamp
 */
final class TimestampNowProc
    extends Procedure0
{
    TimestampNowProc()
    {
        //    "                                                                               |
        super("Returns a timestamp representing \"now\".  At present the local offset is\n" +
              "unspecified, but that may change in the future.");
    }

    @Override
    Object doApply(Evaluator eval)
        throws FusionException
    {
        Timestamp now = Timestamp.now();
        return eval.newTimestamp(now);
    }
}
