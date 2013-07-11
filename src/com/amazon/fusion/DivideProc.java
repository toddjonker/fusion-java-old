// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWriteToString;
import java.math.BigDecimal;


class DivideProc
    extends Procedure
{
    DivideProc()
    {
        //    "                                                                               |
        super("Returns a decimal whose numeric value is `(dividend / divisor)`.  Both\n" +
              "arguments must be decimals.  An exception is thrown if the result cannot be\n" +
              "represented exactly.",
              "dividend", "divisor");
    }


    /**
     * Helper to allow subclass to tweak the divison context.
     */
    BigDecimal divide(Evaluator eval, Object[] args,
                      BigDecimal dividend, BigDecimal divisor)
        throws FusionException
    {
        try
        {
            return dividend.divide(divisor);
        }
        catch (ArithmeticException e)
        {
            String message =
                getInferredName() + ": result of division isn't exact.\n" +
                "Arguments were:\n  " + safeWriteToString(eval, args[0]) +
                "\n  " + safeWriteToString(eval, args[1]);
            throw new ContractFailure(message);
        }
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        BigDecimal dividend = checkRequiredDecimalArg(eval, 0, args);
        BigDecimal divisor  = checkRequiredDecimalArg(eval, 1, args);

        BigDecimal result = divide(eval, args, dividend, divisor);
        return eval.newDecimal(result);
    }
}
