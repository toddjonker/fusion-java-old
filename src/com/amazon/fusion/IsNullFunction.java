// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;

/**
 *
 */
class IsNullFunction
    extends FunctionValue
{
    IsNullFunction()
    {
        //    "                                                                               |
        super("Returns true when VALUE is any Ion null, false otherwise.",
              "value");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        expectArityExact(1, args);

        boolean isNull;
        FusionValue arg = args[0];
        if (arg instanceof DomValue)
        {
            IonValue value = ((DomValue) arg).getDom();
            isNull = value.isNullValue();
        }
        else
        {
            isNull = false;
        }

        return eval.newBool(isNull);
    }
}
