// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
class IsUndefFunction
    extends FunctionValue
{
    IsUndefFunction()
    {
        //    "                                                                               |
        super("Returns true when VALUE is the unique UNDEF object, false otherwise.",
              "value");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        FusionValue arg = args[0];
        boolean isUndef = (arg == UNDEF);
        return eval.newBool(isUndef);
    }
}
