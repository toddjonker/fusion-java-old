// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Numeric difference and negation.
 */
final class DifferenceProc
    extends Procedure
{
    DifferenceProc()
    {
        //    "                                                                               |
        super("With two or more int or decimal `num`bers, returns their difference,\n"
            + "associating to the left.  With one int or decimal argument, returns its\n"
            + "negation.",
              "num", DOTDOTDOTPLUS);
    }


    private static BigDecimal toBigDecimal(Number num)
    {
        if (num instanceof BigDecimal) return (BigDecimal) num;
        return new BigDecimal((BigInteger) num);
    }

    private static Number subtract(Number left, Number right)
        throws FusionException
    {
        Number result;
        if (left instanceof BigInteger && right instanceof BigInteger)
        {
            BigInteger leftInt  = (BigInteger)left;
            BigInteger rightInt = (BigInteger)right;
            result = leftInt.subtract(rightInt);
        }
        else
        {
            BigDecimal leftDec  = toBigDecimal(left);
            BigDecimal rightDec = toBigDecimal(right);
            result = leftDec.subtract(rightDec);
        }

        return result;
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);

        Number result;

        Number num = checkBigArg(0, args);
        if (args.length == 1)
        {
            result = subtract(BigInteger.ZERO, num);
        }
        else
        {
            result = num;
            for (int i = 1; i < args.length; i++)
            {
                num = checkBigArg(i, args);
                result = subtract(result, num);
            }
        }

        return eval.injectMaybe(result);
    }
}
