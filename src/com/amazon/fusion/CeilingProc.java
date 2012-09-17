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
    extends Procedure
{
    CeilingProc()
    {
        //    "                                                                               |
        super("Returns the smallest number larger than or equal to the input number",
              "number");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonValue arg = FusionValue.toIonValue(args[0]);

        Object result = null;

        if (arg instanceof IonInt)
        {
            result = args[0];
        }
        else if (arg instanceof IonDecimal)
        {
            IonDecimal dArg = (IonDecimal)arg;
            BigDecimal dVal = dArg.bigDecimalValue();
            BigInteger iVal = dVal.setScale(0, RoundingMode.CEILING).toBigInteger();
            result = eval.newInt(iVal);
        }

        if (result != null)
        {
            return result;
        }

        throw new ContractFailure("Incorrect type input to perform ceiling operation");
    }

}
