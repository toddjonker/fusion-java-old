// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static java.math.RoundingMode.HALF_EVEN;
import java.math.BigDecimal;


/** EXPERIMENTAL **/
class DecimalDivideRescaleProc
    extends Procedure
{
    DecimalDivideRescaleProc()
    {
        //    "                                                                               |
        super("docs in Fusion");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(3, args);

        BigDecimal dividend = checkRequiredDecimalArg(eval, 0, args);
        BigDecimal divisor  = checkRequiredDecimalArg(eval, 1, args);
        int scale = checkIntArg(2, args);

        BigDecimal result = dividend.divide(divisor, scale, HALF_EVEN);
        return eval.newDecimal(result);
    }
}
