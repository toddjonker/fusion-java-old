// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        boolean isNull = FusionValue.isAnyIonNull(args[0]);
        return eval.newBool(isNull);
    }
}
