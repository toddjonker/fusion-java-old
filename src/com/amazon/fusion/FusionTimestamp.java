// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.math.BigDecimal;


final class FusionTimestamp
{
    private FusionTimestamp() {}


    static final class TimestampNowProc
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


    static final class TimestampToEpochMillisProc
        extends Procedure1
    {
        TimestampToEpochMillisProc()
        {
            //    "                                                                               |
            super("Given a non-null timestamp, returns the same point in time represented as the\n" +
                  "number of milliseconds since 1970-01-01T00:00Z.  The result is a decimal.",
                  "timestamp");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            IonValue iv = FusionValue.castToIonValueMaybe(arg);
            if (iv instanceof IonTimestamp)
            {
                Timestamp ts = ((IonTimestamp) iv).timestampValue();
                if (ts != null)
                {
                    BigDecimal millis = ts.getDecimalMillis();
                    return eval.newDecimal(millis);
                }
            }

            throw new ArgTypeFailure(this, "non-null timestamp", 0, arg);
        }
    }
}
