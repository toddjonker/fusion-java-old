// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.math.BigDecimal;
import java.math.BigInteger;


/**
 * Numeric product.
 */
final class ProductProc
    extends Procedure
{
    ProductProc()
    {
        //    "                                                                               |
        super("Returns the product of the `num`bers, which must be int or decimal.  With no\n" +
              "arguments, returns integer 1.",
              "num", DOTDOTDOT);
    }

    private static BigDecimal toBigDecimal(Number num)
    {
        if (num instanceof BigDecimal) return (BigDecimal) num;
        return new BigDecimal((BigInteger) num);
    }

    private static Number multiply(Number left, Number right)
        throws FusionException
    {
        Number result;
        if (left instanceof BigInteger && right instanceof BigInteger)
        {
            BigInteger leftInt  = (BigInteger)left;
            BigInteger rightInt = (BigInteger)right;
            result = leftInt.multiply(rightInt);
        }
        else
        {
            BigDecimal leftDec  = toBigDecimal(left);
            BigDecimal rightDec = toBigDecimal(right);
            result = leftDec.multiply(rightDec);
        }

        return result;
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        Number result = BigInteger.ONE;

        for (int i = 0; i < args.length; i++)
        {
            Number num = checkBigArg(i, args);
            result = multiply(result, num);
        }

        return eval.injectMaybe(result);
    }
}
