// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;
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
    Object doApply(Evaluator eval, Object arg0, Object arg1)
        throws FusionException
    {
        IonValue iv0 = copyToIonValueMaybe(arg0, eval.getSystem());
        IonValue iv1 = copyToIonValueMaybe(arg1, eval.getSystem());

        boolean result = iv0 != null && iv0.equals(iv1);
        return makeBool(eval, result);
    }
}
