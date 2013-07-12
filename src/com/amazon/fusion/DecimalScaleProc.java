// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.math.BigDecimal;


/** EXPERIMENTAL **/
final class DecimalScaleProc
    extends Procedure
{
    DecimalScaleProc()
    {
        //    "                                                                               |
        super("docs in Fusion");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        BigDecimal value = checkRequiredDecimalArg(eval, 0, args);

        int result = value.scale();
        return eval.newInt(result);
    }
}
