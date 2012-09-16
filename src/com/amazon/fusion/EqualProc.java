// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonString;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;

/**
 *
 */
final class EqualProc
    extends Procedure
{
    EqualProc()
    {
        //    "                                                                               |
        super("Returns true if the arguments are equal if the arguments are of type \n" +
              "integer, decimal, boolean, string, or timestamp.",
              "value1", "value2");
    }
    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonValue leftVal  = FusionValue.toIonValue(args[0]);
        IonValue rightVal = FusionValue.toIonValue(args[1]);
        boolean result = false;
        int compareVal = 0;

        if (leftVal == null || rightVal == null)
        {
            throw contractFailure("One or more args is not of a valid Ion type."+
                    "Expected: int, decimal, bool, or timestamp; "+
                    "observed: "+ FusionValue.writeToString(args[0]) +
                    " and " + FusionValue.writeToString(args[1]));
        }
        if (leftVal instanceof IonInt && rightVal instanceof IonInt)
        {
            IonInt left  = (IonInt) leftVal;
            IonInt right = (IonInt) rightVal;
            compareVal = left.bigIntegerValue().compareTo(right.bigIntegerValue());
            result = (compareVal == 0);
        } else if (leftVal instanceof IonDecimal && rightVal instanceof IonDecimal)
        {
            IonDecimal left = (IonDecimal) leftVal;
            IonDecimal right = (IonDecimal) rightVal;
            compareVal = left.bigDecimalValue().compareTo(right.bigDecimalValue());
            result = (compareVal == 0);
        } else if (leftVal instanceof IonBool && rightVal instanceof IonBool)
        {
            IonBool left  = (IonBool) leftVal;
            IonBool right = (IonBool) rightVal;
            result = left.booleanValue() == right.booleanValue();
        } else if (leftVal instanceof IonString && rightVal instanceof IonString)
        {
            IonString left  = (IonString) leftVal;
            IonString right = (IonString) rightVal;
            result = left.stringValue().equals(right.stringValue());
        } else if (leftVal instanceof IonTimestamp && rightVal instanceof IonTimestamp)
        {
            IonTimestamp left = (IonTimestamp) leftVal;
            IonTimestamp right = (IonTimestamp) rightVal;
            compareVal = left.timestampValue().compareTo(right.timestampValue());
            result = (compareVal == 0);
        } else
        {
            throw contractFailure("One or more args is not of a valid Ion type."+
                                  "Expected: int, decimal, bool, or timestamp; "+
                                  "observed: "+ FusionValue.writeToString(args[0]) +
                                  " and " + FusionValue.writeToString(args[1]));
        }

        return eval.newBool(result);
    }
}
