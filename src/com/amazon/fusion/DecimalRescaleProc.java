// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;
import static com.amazon.fusion.FusionNumber.checkRequiredDecimalArg;
import static com.amazon.fusion.FusionNumber.makeDecimal;
import java.math.BigDecimal;
import java.math.RoundingMode;


/** EXPERIMENTAL **/
final class DecimalRescaleProc
    extends Procedure
{
    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        BigDecimal value = checkRequiredDecimalArg(eval, this, 0, args);
        int        scale = checkIntArgToJavaInt   (eval, this, 1, args);

        BigDecimal result = value.setScale(scale, RoundingMode.HALF_EVEN);
        return makeDecimal(eval, result);
    }
}
