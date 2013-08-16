// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class IsNullNullProc
    extends Procedure1
{
    IsNullNullProc()
    {
        //    "                                                                               |
        super("Returns `true` when `value` is `null.null`, `false` otherwise.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean isNull = FusionValue.isNullNull(eval, arg);
        return eval.newBool(isNull);
    }
}
