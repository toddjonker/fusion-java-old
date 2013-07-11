// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.math.BigDecimal;
import java.math.RoundingMode;


/** EXPERIMENTAL **/
final class DecimalRescaleProc
    extends Procedure
{
    DecimalRescaleProc()
    {
        //    "                                                                               |
        super("docs in Fusion");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        BigDecimal value = checkRequiredDecimalArg(eval, 0, args);
        int scale = checkIntArg(1, args);

        BigDecimal result = value.setScale(scale, RoundingMode.HALF_EVEN);
        return eval.newDecimal(result);
    }
}
