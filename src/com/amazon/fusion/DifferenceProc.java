// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Numeric difference and negation.
 */
final class DifferenceProc
    extends Procedure
{
    DifferenceProc()
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
        checkArityAtLeast(1, args);

        long result = checkLongArg(0, args);

        int arity = args.length;
        if (arity == 1)
        {
            result = -result;
        }
        else
        {
            for (int i = 1; i < args.length; i++)
            {
                long v = checkLongArg(i, args);
                result -= v;
            }
        }

        return eval.newInt(result);
    }
}
