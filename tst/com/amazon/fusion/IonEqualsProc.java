// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;

/** FOR TESTING ONLY */
final class IonEqualsProc
    extends Procedure2
{
    IonEqualsProc()
    {
        //    "                                                                               |
        super("Compares two ionizable values.",
              "value1", "value2");
    }

    @Override
    @SuppressWarnings("deprecation")
    Object doApply(Evaluator eval, Object arg0, Object arg1)
        throws FusionException
    {
        IonValue iv0 = eval.convertToIonValueMaybe(arg0);
        IonValue iv1 = eval.convertToIonValueMaybe(arg1);

        boolean result = iv0 != null && iv0.equals(iv1);
        return eval.newBool(result);
    }
}
