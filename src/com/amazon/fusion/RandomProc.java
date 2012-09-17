// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;



/**
 * Random number generator
 */
final class RandomProc
    extends Procedure
{
    RandomProc()
    {
        //    "                                                                               |
        super("Returns a random decimal between 0 and 1.");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        double result = Math.random();

        return eval.newDecimal(result);
    }
}
