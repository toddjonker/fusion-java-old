// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;

/**
 *
 */
final class LessThanProc
    extends Procedure
{
    LessThanProc()
    {
        //    "                                                                               |
        super("Returns true if the left arg is strictly less than the right arg if the" +
                " args are both of the same type, with int and timestamps being the only" +
                " valid ones.");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        IonValue leftVal  = FusionValue.toIonValue(args[0]);
        IonValue rightVal = FusionValue.toIonValue(args[1]);
        boolean result = false;
        FusionValue fusionResult;

        if (leftVal == null || rightVal == null)
        {
            emitContractFailure();
        }
        if (leftVal instanceof IonInt && rightVal instanceof IonInt)
        {
            IonInt left  = (IonInt) leftVal;
            IonInt right = (IonInt) rightVal;
            result = left.longValue() < right.longValue();
        } else if (leftVal instanceof IonTimestamp && rightVal instanceof IonTimestamp)
        {
            IonTimestamp left = (IonTimestamp) leftVal;
            IonTimestamp right = (IonTimestamp) rightVal;
            int compareVal = left.timestampValue().compareTo(right.timestampValue());
            result = (compareVal == -1);
        } else
        {
            emitContractFailure();
        }
        fusionResult = eval.newBool(result);
        return fusionResult;

    }
}
