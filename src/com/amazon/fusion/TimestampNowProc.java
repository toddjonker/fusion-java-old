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
        super("Returns a timestamp representing \"now\".");
    }

    @Override
    Object doApply(Evaluator eval)
        throws FusionException
    {
        Timestamp now = Timestamp.now();
        return eval.newTimestamp(now);
    }
}
