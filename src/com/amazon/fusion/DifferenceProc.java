// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.ZERO_INT;
import com.amazon.fusion.FusionNumber.BaseDecimal;
import com.amazon.fusion.FusionNumber.BaseInt;
import com.amazon.fusion.FusionNumber.BaseNumber;

/**
 * Numeric difference and negation.
 */
final class DifferenceProc
    extends Procedure
{
    DifferenceProc()
    {
        //    "                                                                               |
        super("With two or more int or decimal `num`bers, returns their difference,\n"
            + "associating to the left.  With one int or decimal argument, returns its\n"
            + "negation.",
              "num", DOTDOTDOTPLUS);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);

        Object arg0 = args[0];
        if (arg0 instanceof BaseInt || arg0 instanceof BaseDecimal)
        {
            BaseNumber accum = (BaseNumber) arg0;

            int arity = args.length;
            if (arity == 1)
            {
                return ZERO_INT.subtract(accum);
            }
            else
            {
                for (int i = 1; i < arity; i++)
                {
                    Object arg = args[i];
                    if (arg instanceof BaseInt || arg instanceof BaseDecimal)
                    {
                        BaseNumber num = (BaseNumber) arg;
                        if (! num.isAnyNull())
                        {
                            accum = accum.subtract(num);
                            continue;
                        }
                    }

                    throw argFailure("non-null int or decimal", i, args);
                }

                return accum;
            }
        }

        throw argFailure("non-null int or decimal", 0, args);
    }
}
