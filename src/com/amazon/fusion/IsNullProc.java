// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


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

        boolean isNull = FusionValue.isAnyIonNull(args[0]);
        return eval.newBool(isNull);
    }
}
