// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

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
        super("With two or more arguments, returns their difference, associating to the\n" +
              "left. With one argument, returns its negation.",
              "int or dec", DOTDOTDOTPLUS);
    }

    Number subtract(Number result, Number operand)
            throws FusionException
        {
            Number newResult = null;
            if (result instanceof BigInteger && operand instanceof BigInteger)
            {
                BigInteger op1 = (BigInteger)result;
                BigInteger op2 = (BigInteger)operand;
                newResult = op1.subtract(op2);
            }
            if (result instanceof BigInteger && operand instanceof BigDecimal)
            {
                BigDecimal bResult = new BigDecimal((BigInteger)result);
                newResult = bResult.subtract((BigDecimal)operand);
            } else if (result instanceof BigDecimal && operand instanceof BigInteger)
            {
                BigDecimal bOperand = new BigDecimal((BigInteger)operand);
                newResult = ((BigDecimal)result).subtract(bOperand);
            }
            else if (result instanceof BigDecimal && operand instanceof BigDecimal)
            {
                BigDecimal op1 = (BigDecimal)result;
                BigDecimal op2 = (BigDecimal)operand;
                newResult = op1.subtract(op2);
            }

            if (newResult == null)
            {
                emitContractFailure("int or decimal", operand);
            }

            return newResult;
        }

        @Override
        FusionValue invoke(Evaluator eval, FusionValue[] args)
            throws FusionException
        {
            checkArityAtLeast(1,args);

            FusionValue finalResult = null;
            Number result;

            Number operandNum = checkBigArg(0, args);
            if (args.length == 1)
            {
                result = subtract(BigInteger.ZERO,operandNum);
            } else
            {
                result = operandNum;
                for (int i = 1; i < args.length; i++)
                {
                    operandNum = checkBigArg(i, args);
                    result = subtract(result,operandNum);
                }
            }

            if (result instanceof BigInteger)
            {
                BigInteger iResult = (BigInteger)result;
                finalResult = eval.newInt(iResult);
            } else if (result instanceof BigDecimal)
            {
                BigDecimal dResult = (BigDecimal)result;
                finalResult = eval.newDecimal(dResult);
            }

            return finalResult;
        }
}
