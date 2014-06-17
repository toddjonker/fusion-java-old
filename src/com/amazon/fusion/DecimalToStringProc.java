// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkNullableDecimalArg;
import static com.amazon.fusion.FusionString.makeString;
import com.amazon.ion.util.IonTextUtils;
import java.math.BigDecimal;

final class DecimalToStringProc
    extends Procedure
{
    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        BigDecimal val = checkNullableDecimalArg(eval, this, 0, args);
        String text = null;
        if (val != null)
        {
            text = IonTextUtils.printDecimal(val);
        }
        return makeString(eval, text);
    }
}
