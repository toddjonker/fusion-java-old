// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkRequiredDecimalArg;
import static com.amazon.fusion.FusionNumber.makeInt;
import java.math.BigDecimal;


/** EXPERIMENTAL **/
final class DecimalScaleProc
    extends Procedure
{
    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        BigDecimal value = checkRequiredDecimalArg(eval, this, 0, args);

        int result = value.scale();
        return makeInt(eval, result);
    }
}
