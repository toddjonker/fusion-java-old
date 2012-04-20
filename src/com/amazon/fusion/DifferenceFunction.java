// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Numeric difference and negation.
 */
final class DifferenceFunction
    extends FunctionValue
{
    DifferenceFunction()
    {
        //    "                                                                               |
        super("With two or more (int) arguments, returns their difference, associating to the\n" +
              "left. With one (int) argument, returns its negation.",
              "int", DOTDOTDOTPLUS);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        expectArityAtLeast(1, args);

        long result = assumeLongArg(0, args);

        int arity = args.length;
        if (arity == 1)
        {
            result = -result;
        }
        else
        {
            for (int i = 1; i < args.length; i++)
            {
                long v = assumeLongArg(i, args);
                result -= v;
            }
        }

        return eval.newInt(result);
    }
}
