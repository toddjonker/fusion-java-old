// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class IsUndefProc
    extends Procedure1
{
    IsUndefProc()
    {
        //    "                                                                               |
        super("Returns true when VALUE is the unique UNDEF object, false otherwise.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean isUndef = (arg == UNDEF);
        return eval.newBool(isUndef);
    }
}
