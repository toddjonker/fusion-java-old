// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.isSexp;
import com.amazon.ion.IonSexp;

final class IsSexpProc
    extends Procedure1
{
    IsSexpProc()
    {
        //    "                                                                               |
        super("Determines whether a VALUE is an sexp, returning true or false.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean result = (isSexp(eval, arg) || arg instanceof IonSexp);
        return eval.newBool(result);
    }
}
