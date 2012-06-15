// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
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
        super("Returns true if the arguments are equal if the a arguments are of type \n" +
              "integer, boolean, string, or timestamp.");
    }

    void emitContractFailure()
        throws FusionException
    {
        throw new ContractFailure("Mismatched or invalid type equivalence cannot be determined.");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        IonValue leftVal = args[0].ionValue();
        IonValue rightVal = args[1].ionValue();
        boolean result = false;
        IonBool resultDom = null;

        if (leftVal == null || rightVal == null)
        {
            emitContractFailure();
        }
        if (leftVal instanceof IonInt && rightVal instanceof IonInt)
        {
            IonInt left  = (IonInt) leftVal;
            IonInt right = (IonInt) rightVal;
            result = left.longValue() == right.longValue();
            resultDom = left.getSystem().newBool(result);
        } else if (leftVal instanceof IonBool && rightVal instanceof IonBool)
        {
            IonBool left  = (IonBool) leftVal;
            IonBool right = (IonBool) rightVal;
            result = left.booleanValue() == right.booleanValue();
            resultDom = left.getSystem().newBool(result);
        } else if (leftVal instanceof IonString && rightVal instanceof IonString)
        {
            IonString left  = (IonString) leftVal;
            IonString right = (IonString) rightVal;
            result = left.stringValue().equals(right.stringValue());
            resultDom = left.getSystem().newBool(result);
        } else if (leftVal instanceof IonTimestamp && rightVal instanceof IonTimestamp)
        {
            IonTimestamp left = (IonTimestamp) leftVal;
            IonTimestamp right = (IonTimestamp) rightVal;
            int compareVal = left.timestampValue().compareTo(right.timestampValue());
            result = (compareVal == 0);
            resultDom = left.getSystem().newBool(result);
        } else
        {
            emitContractFailure();
        }

        return new DomValue(resultDom);
    }
}
