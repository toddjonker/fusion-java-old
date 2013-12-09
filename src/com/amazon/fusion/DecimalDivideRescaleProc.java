// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;
import static com.amazon.fusion.FusionNumber.checkRequiredDecimalArg;
import static com.amazon.fusion.FusionNumber.makeDecimal;
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

        BigDecimal dividend = checkRequiredDecimalArg(eval, this, 0, args);
        BigDecimal divisor  = checkRequiredDecimalArg(eval, this, 1, args);
        int        scale    = checkIntArgToJavaInt   (eval, this, 2, args);

        BigDecimal result = dividend.divide(divisor, scale, HALF_EVEN);
        return makeDecimal(eval, result);
    }
}
