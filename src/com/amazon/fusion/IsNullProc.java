// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;

final class IsNullProc
    extends Procedure1
{
    IsNullProc()
    {
        //    "                                                                               |
        super("Returns `true` when `value` is _any_ Ion null, `false` otherwise.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean isNull = FusionValue.isAnyNull(eval, arg);
        return makeBool(eval, isNull);
    }
}
