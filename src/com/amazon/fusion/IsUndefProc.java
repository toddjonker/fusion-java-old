// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class IsUndefProc
    extends Procedure
{
    IsUndefProc()
    {
        //    "                                                                               |
        super("Returns true when VALUE is the unique UNDEF object, false otherwise.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        boolean isUndef = (args[0] == UNDEF);
        return eval.newBool(isUndef);
    }
}
