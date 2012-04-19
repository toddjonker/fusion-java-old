// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;

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
    {
        FusionValue arg = args[0];
        boolean isUndef = (arg == UNDEF);
        IonValue result = eval.getSystem().newBool(isUndef);
        return new DomValue(result);
    }
}
