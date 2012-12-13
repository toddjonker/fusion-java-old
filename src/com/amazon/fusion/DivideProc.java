// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionPrint.safeWriteToString;
import java.math.BigDecimal;


final class DivideProc
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

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        BigDecimal dividend = checkRequiredDecimalArg(eval, 0, args);
        BigDecimal divisor  = checkRequiredDecimalArg(eval, 1, args);

        try
        {
            BigDecimal result = dividend.divide(divisor);
            return eval.newDecimal(result);
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
}
