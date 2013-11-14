// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.util.IonTextUtils.isDigit;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.math.BigDecimal;


final class FusionTimestamp
{
    private FusionTimestamp() {}


    static Timestamp checkNullableArg(Procedure who, int argNum,
                                      Object... args)
        throws ArgTypeFailure
    {
        IonTimestamp iv = who.checkDomArg(IonTimestamp.class, "timestamp",
                                          true /* nullable */, argNum, args);
        return iv.timestampValue();
    }


    static final class StringToTimestampProc
        extends Procedure
    {
        StringToTimestampProc()
        {
            //    "                                                                               |
            super("Converts a `string` to a timestamp, recognizing (only) Ion formatted data.\n" +
                  "Returns `null.timestamp` when given `null.string`.\n" +
                  "\n" +
                  "    (string_to_timestamp null.string)        ==> null.timestamp\n" +
                  "    (string_to_timestamp \"2013-11-13T\")      ==> 2013-11-13\n" +
                  "    (string_to_timestamp \"null.timestamp\")   ==> ERROR",
                  "string");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkNullableStringArg(0, args);

            Timestamp result;
            if (input == null)
            {
                result = null;
            }
            else
            {
                try
                {
                    result = Timestamp.valueOf(input);

                    // Don't accept input "null.timestamp". That's shady.
                    if (result == null)
                    {
                        throw argFailure("timestamp-formatted string", 0, args);
                    }

                    // Hack around Timestamp.valueOf() accepting stoppers.
                    char last = input.charAt(input.length() - 1);
                    if (! (last == 'Z' || last == 'T' || isDigit(last, 10)))
                    {
                        throw argFailure("timestamp-formatted string", 0, args);
                    }
                }
                catch (IllegalArgumentException e)
                {
                    throw argFailure("timestamp-formatted string", 0, args);
                }
            }

            return eval.newTimestamp(result);
        }
    }


    static final class TimestampToStringProc
        extends Procedure
    {
        TimestampToStringProc()
        {
            //    "                                                                               |
            super("Converts a `timestamp` to a string in Ion format.\n" +
                  "Returns `null.string` when given `null.timestamp`.\n" +
                  "\n" +
                  "    (timestamp_to_string null.timestamp)   ==> null.string\n" +
                  "    (timestamp_to_string 2013-11-13T)      ==> \"2013-11-13\"",
                  "timestamp");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Timestamp input = checkNullableArg(this, 0, args);

            String result = (input == null ? null : input.toString());
            return eval.newString(result);
        }
    }


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
