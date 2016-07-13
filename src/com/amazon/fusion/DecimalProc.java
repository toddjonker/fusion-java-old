// Copyright (c) 2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkNullableNumberArgToJavaBigDecimal;
import static com.amazon.fusion.FusionNumber.checkRequiredIntArg;
import static com.amazon.fusion.FusionNumber.isFloat;
import static com.amazon.fusion.FusionNumber.makeDecimal;
import java.math.BigDecimal;

final class DecimalProc
    extends Procedure
{
    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        final BigDecimal coefficient;
        checkArityRange(1, 2, args);

        final int exponent;
        if (args.length == 2) {
            exponent = checkRequiredIntArg(eval, this, 1, args).intValue();
        } else {
            exponent = 0;
        }

        try {
            coefficient = checkNullableNumberArgToJavaBigDecimal(eval, this, 0, args);

            if (coefficient == null) {
                return makeDecimal(eval, null);
            }
        } catch (final NumberFormatException exception) {
            final Object rawCoefficient = args[0];
            assert isFloat(eval, rawCoefficient);
            throw argFailure("unable to convert non-number float in coefficient", 0, args);
        }

        return makeDecimal(eval, coefficient.scaleByPowerOfTen(exponent));
    }

}
