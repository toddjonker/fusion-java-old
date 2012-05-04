// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Numeric sum.
 */
final class SumProc
    extends Procedure
{
    SumProc()
    {
        //    "                                                                               |
        super("Returns the sum of the (integer) arguments. With no arguments, returns 0.",
              "int", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        long result = 0;

        for (int i = 0; i < args.length; i++)
        {
            long v = checkLongArg(i, args);
            result += v;
        }

        return eval.newInt(result);
    }
}
