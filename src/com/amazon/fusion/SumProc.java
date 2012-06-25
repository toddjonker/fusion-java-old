// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

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
        super("Returns the sum of the arguments. With no arguments, returns 0.",
              "int or dec", DOTDOTDOT);
    }

    Number add(Number result, Number operand)
        throws FusionException
    {
        Number newResult = null;
        if (result instanceof BigInteger && operand instanceof BigInteger)
        {
            BigInteger op1 = (BigInteger)result;
            BigInteger op2 = (BigInteger)operand;
            newResult = op1.add(op2);
        } else if (result instanceof BigInteger && operand instanceof BigDecimal)
        {
            BigDecimal bResult = new BigDecimal((BigInteger)result);
            newResult = bResult.add((BigDecimal)operand);
        } else if (result instanceof BigDecimal && operand instanceof BigInteger)
        {
            BigDecimal bOperand = new BigDecimal((BigInteger)operand);
            newResult = bOperand.add((BigDecimal)result);
        } else if (result instanceof BigDecimal && operand instanceof BigDecimal)
        {
            BigDecimal op1 = (BigDecimal)result;
            BigDecimal op2 = (BigDecimal)operand;
            newResult = op1.add(op2);
        }

        return newResult;
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        FusionValue finalResult = null;
        Number result = BigInteger.ZERO;

        for (int i = 0; i < args.length; i++)
        {
            Number operandNum = checkBigArg(i, args);
            if (operandNum == null)
            {
                throw contractFailure("Expected: int or decimal; observed: "+
                                      FusionValue.writeToString(args[i]));
            }
            result = add(result,operandNum);
        }

        if (result instanceof BigInteger)
        {
            finalResult = eval.newInt((BigInteger)result);
        } else if (result instanceof BigDecimal)
        {
            finalResult = eval.newDecimal((BigDecimal)result);
        }

        return finalResult;
    }
}
