// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;



/**
 * Random number generator
 */
final class RandomProc
    extends Procedure0
{
    RandomProc()
    {
        //    "                                                                               |
        super("Returns a random decimal between 0 and 1.");
    }

    @Override
    Object doApply(Evaluator eval)
        throws FusionException
    {
        double result = Math.random();

        return eval.newDecimal(result);
    }
}
