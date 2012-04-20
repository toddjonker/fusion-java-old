// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Numeric product.
 */
final class ProductFunction
    extends FunctionValue
{
    ProductFunction()
    {
        //    "                                                                               |
        super("Returns the product of the (integer) arguments. With no arguments, returns 1.",
              "int", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        long result = 1;

        for (int i = 0; i < args.length; i++)
        {
            long v = assumeLongArg(i, args);
            result *= v;
        }

        return eval.newInt(result);
    }
}
