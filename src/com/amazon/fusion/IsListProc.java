// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.isVector;
import com.amazon.ion.IonList;

final class IsListProc
    extends Procedure1
{
    IsListProc()
    {
        //    "                                                                               |
        super("Determines whether a VALUE is an Ion list, returning true or false.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean result = (isVector(eval, arg) || arg instanceof IonList);
        return eval.newBool(result);
    }
}
