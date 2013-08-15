// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * Numeric sum.
 */
final class SumProc
    extends Procedure
{
    SumProc()
    {
        //    "                                                                               |
        super("Returns the sum of the `num`bers, which must be int or decimal.  With no\n"
            + "arguments, returns integer 0.",
              "num", DOTDOTDOT);
    }

    private static BigDecimal toBigDecimal(Number num)
    {
        if (num instanceof BigDecimal) return (BigDecimal) num;
        return new BigDecimal((BigInteger) num);
    }

    private static Number add(Number left, Number right)
        throws FusionException
    {
        Number result;
        if (left instanceof BigInteger && right instanceof BigInteger)
        {
            BigInteger leftInt  = (BigInteger)left;
            BigInteger rightInt = (BigInteger)right;
            result = leftInt.add(rightInt);
        }
        else
        {
            BigDecimal leftDec  = toBigDecimal(left);
            BigDecimal rightDec = toBigDecimal(right);
            result = leftDec.add(rightDec);
        }

        return result;
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        Number result = BigInteger.ZERO;

        for (int i = 0; i < args.length; i++)
        {
            Number num = checkBigArg(i, args);
            result = add(result, num);
        }

        return eval.injectMaybe(result);
    }
}
