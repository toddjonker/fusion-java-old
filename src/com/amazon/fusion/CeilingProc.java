// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.isDecimal;
import static com.amazon.fusion.FusionNumber.isInt;
import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionNumber.unsafeNumberToBigDecimal;
import static java.math.RoundingMode.CEILING;
import java.math.BigDecimal;
import java.math.BigInteger;


/**
 * Returns the smallest number larger than or equal to the input number
 */
final class CeilingProc
    extends Procedure1
{
    CeilingProc()
    {
        //    "                                                                               |
        super("Returns the smallest int greater than or equal to `number` (that is, truncate\n"
            + "toward positive infinity). The input must be a non-null int or decimal, and\n"
            + "the result is an int.",
              "number");
    }

    @Override
    Object doApply(Evaluator eval, Object arg0)
        throws FusionException
    {
        if (! isAnyNull(eval, arg0))
        {
            if (isDecimal(eval, arg0))
            {
                BigDecimal d = unsafeNumberToBigDecimal(eval, arg0);
                BigInteger i = d.setScale(0, CEILING).toBigInteger();
                return makeInt(eval, i);
            }

            if (isInt(eval, arg0))
            {
                return arg0;
            }
        }

        throw new ArgTypeFailure(this, "non-null int or decimal", 0, arg0);
    }
}
