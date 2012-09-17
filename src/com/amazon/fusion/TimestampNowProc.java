package com.amazon.fusion;

import com.amazon.ion.IonTimestamp;

/*
 * Generates the current timestamp
 */
final class TimestampNowProc
    extends Procedure
{
    TimestampNowProc()
    {
        super("Generates the current timestamp");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonTimestamp ionTime = eval.getSystem().newCurrentUtcTimestamp();

        return new DomValue(ionTime);

    }
}
