// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;
import static com.amazon.fusion.FusionString.checkRequiredStringArg;
import static com.amazon.fusion.FusionText.checkRequiredTextArg;
import java.util.Arrays;


final class RaiseArgumentErrorProc
    extends Procedure
{
    RaiseArgumentErrorProc()
    {
        //    "                                                                               |
        super("Raises a contract failure due to a procedure being called with unacceptable\n" +
              "argument values.  The `name` text (string or symbol) identifies the procedure.\n" +
              "The `expected` string describes the relevant contract.  `bad_pos` is the\n" +
              "zero-based position of the bad argument; a negative value means that a specific\n" +
              "argument is not implicated.  The `value`s are the arguments passed to the\n" +
              "procedure.",
              "name", "expected", "bad_pos", "value", DOTDOTDOTPLUS);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(4, args);

        String name     = checkRequiredTextArg(eval, this, 0, args);
        String expected = checkRequiredStringArg(eval, this, 1, args);
        int    badPos   = checkIntArgToJavaInt(eval, this, 2, args);

        Object[] actuals = Arrays.copyOfRange(args, 3, args.length);

        if (name.isEmpty()) name = "unknown procedure";

        if (actuals.length <= badPos)
        {
            // The position is bad, but we don't want to blow up because these
            // code paths are generally untested. Better to act like we don't
            // know which argument is bad; at least an error message is given.
            badPos = -1;
        }

        throw new ArgTypeFailure(name, expected, badPos, actuals);
    }
}
