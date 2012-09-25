// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonValue;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;


/**
 * Returns the largest number smaller than or equal to the input number
 */
final class FloorProc
    extends Procedure
{
    FloorProc()
    {
        //    "                                                                               |
        super("Returns the largest number smaller than or equal to the input number");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1,args);

        IonValue arg = FusionValue.castToIonValueMaybe(args[0]);

        if (arg instanceof IonInt)
        {
            return args[0];
        }

        if (arg instanceof IonDecimal)
        {
            IonDecimal dArg = (IonDecimal)arg;
            BigDecimal dVal = dArg.bigDecimalValue();
            BigInteger iVal = dVal.setScale(0, RoundingMode.FLOOR).toBigInteger();
            return eval.newInt(iVal);
        }

        throw new ContractFailure("Incorrect type input to perform ceiling operation");
    }
}
