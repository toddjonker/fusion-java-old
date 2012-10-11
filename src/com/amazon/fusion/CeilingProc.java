// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonValue;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;


/**
 * Returns the smallest number larger than or equal to the input number
 */
final class CeilingProc
    extends Procedure1
{
    CeilingProc()
    {
        //    "                                                                               |
        super("Returns the smallest number larger than or equal to the input number",
              "number");
    }

    @Override
    Object doApply(Evaluator eval, Object arg0)
        throws FusionException
    {
        IonValue arg = FusionValue.castToIonValueMaybe(arg0);

        if (arg instanceof IonInt)
        {
            return arg0;
        }

        if (arg instanceof IonDecimal)
        {
            IonDecimal dArg = (IonDecimal)arg;
            BigDecimal dVal = dArg.bigDecimalValue();
            BigInteger iVal = dVal.setScale(0, RoundingMode.CEILING).toBigInteger();
            return eval.newInt(iVal);
        }

        throw new ContractFailure("Incorrect type input to perform ceiling operation");
    }

}
