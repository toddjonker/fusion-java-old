// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class IsNullProc
    extends Procedure1
{
    IsNullProc()
    {
        //    "                                                                               |
        super("Returns true when VALUE is any Ion null, false otherwise.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean isNull = FusionValue.isAnyNull(eval, arg);
        return eval.newBool(isNull);
    }
}
