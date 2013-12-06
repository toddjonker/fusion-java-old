// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.ZERO_INT;
import com.amazon.fusion.FusionNumber.BaseDecimal;
import com.amazon.fusion.FusionNumber.BaseInt;
import com.amazon.fusion.FusionNumber.BaseNumber;

/**
 * Numeric sum.
 */
final class SumProc
    extends Procedure
{
    SumProc()
    {
        //    "                                                                               |
        super("Returns the sum of the `num`bers, which must be int or decimal.  With no\n"
            + "arguments, returns integer 0.",
              "num", DOTDOTDOT);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        int arity = args.length;

        if (arity == 0) return ZERO_INT;

        Object arg0 = args[0];
        if (arg0 instanceof BaseInt || arg0 instanceof BaseDecimal)
        {
            if (arity == 1) return arg0;

            BaseNumber accum = (BaseNumber) arg0;

            for (int i = 1; i < arity; i++)
            {
                Object arg = args[i];
                if (arg instanceof BaseInt || arg instanceof BaseDecimal)
                {
                    BaseNumber num = (BaseNumber) arg;
                    if (! num.isAnyNull())
                    {
                        accum = accum.add(num);
                        continue;
                    }
                }

                throw argFailure("non-null int or decimal", i, args);
            }

            return accum;
        }

        throw argFailure("non-null int or decimal", 0, args);
    }
}
