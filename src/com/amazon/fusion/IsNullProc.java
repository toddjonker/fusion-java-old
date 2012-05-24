// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;

/**
 *
 */
final class IsNullProc
    extends Procedure
{
    IsNullProc()
    {
        //    "                                                                               |
        super("Returns true when VALUE is any Ion null, false otherwise.",
              "value");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        boolean isNull;
        FusionValue arg = args[0];
        if (arg.isIon())
        {
            IonValue value = arg.ionValue();
            isNull = value.isNullValue();
        }
        else
        {
            isNull = false;
        }

        return eval.newBool(isNull);
    }
}
