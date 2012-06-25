// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;

/**
 *
 */
final class GreaterThanProc
    extends Procedure
{
    GreaterThanProc()
    {
        //    "                                                                               |
        super("Returns true if the left arg is strictly greater than the right arg if the" +
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
        int compareVal = 0;

        if (leftVal == null || rightVal == null)
        {
            throw contractFailure("One or more arguments are not IonValues: "+
                                  "Expected: int, decimal, or timestamp; "+
                                  "observed: "+ FusionValue.writeToString(args[0]) +
                                  " and " + FusionValue.writeToString(args[1]));
        }
        if (leftVal instanceof IonInt && rightVal instanceof IonInt)
        {
            IonInt left  = (IonInt) leftVal;
            IonInt right = (IonInt) rightVal;
            compareVal = left.bigIntegerValue().compareTo(right.bigIntegerValue());
        } else if (leftVal instanceof IonDecimal && rightVal instanceof IonDecimal)
        {
            IonDecimal left = (IonDecimal) leftVal;
            IonDecimal right = (IonDecimal) rightVal;
            compareVal = left.bigDecimalValue().compareTo(right.bigDecimalValue());
        } else if (leftVal instanceof IonTimestamp && rightVal instanceof IonTimestamp)
        {
            IonTimestamp left = (IonTimestamp) leftVal;
            IonTimestamp right = (IonTimestamp) rightVal;
            compareVal = left.timestampValue().compareTo(right.timestampValue());
        } else
        {
            throw contractFailure("One or more args is not of a valid Ion type."+
                                  "Expected: int, decimal, or timestamp; "+
                                  "observed: "+ FusionValue.writeToString(args[0]) +
                                  " and " + FusionValue.writeToString(args[1]));
        }
        result = (compareVal == 1);
        fusionResult = eval.newBool(result);
        return fusionResult;

    }
}
