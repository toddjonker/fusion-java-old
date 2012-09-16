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
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(args);
        FusionValue arg = args[0];
        boolean isUndef = (arg == UNDEF);
        return eval.newBool(isUndef);
    }
}
