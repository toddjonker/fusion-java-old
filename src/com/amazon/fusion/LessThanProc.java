// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;

final class LessThanProc
    extends Procedure
{
    LessThanProc()
    {
        //    "                                                                               |
        super("Returns true if the left arg is strictly less than the right arg if the" +
              " args are both of the same type, with int and timestamps being the only" +
              " valid ones.",
              "value1", "value2");
    }

    private static final String EXPECTATION =
        "non-null int, decimal, or timestamp";

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonValue leftVal  = FusionValue.castToIonValueMaybe(args[0]);
        IonValue rightVal = FusionValue.castToIonValueMaybe(args[1]);
        boolean result = false;
        int compareVal = 0;

        if (leftVal  == null || leftVal.isNullValue() ||
            rightVal == null || rightVal.isNullValue())
        {
            throw new ArgTypeFailure(this, EXPECTATION, -1, args);
        }

        if (leftVal instanceof IonInt && rightVal instanceof IonInt)
        {
            IonInt left  = (IonInt) leftVal;
            IonInt right = (IonInt) rightVal;
            compareVal = left.bigIntegerValue().compareTo(right.bigIntegerValue());
        }
        else if (leftVal instanceof IonDecimal && rightVal instanceof IonDecimal)
        {
            IonDecimal left = (IonDecimal) leftVal;
            IonDecimal right = (IonDecimal) rightVal;
            compareVal = left.bigDecimalValue().compareTo(right.bigDecimalValue());
        }
        else if (leftVal instanceof IonTimestamp && rightVal instanceof IonTimestamp)
        {
            IonTimestamp left = (IonTimestamp) leftVal;
            IonTimestamp right = (IonTimestamp) rightVal;
            compareVal = left.timestampValue().compareTo(right.timestampValue());
        }
        else
        {
            throw new ArgTypeFailure(this, EXPECTATION, -1, args);
        }

        result = (compareVal == -1);
        return eval.newBool(result);
    }
}
