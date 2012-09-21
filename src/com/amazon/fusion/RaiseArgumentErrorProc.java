// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Arrays;


final class RaiseArgumentErrorProc
    extends Procedure
{
    RaiseArgumentErrorProc()
    {
        //    "                                                                               |
        super("Raises a contract failure due to a procedure being called with unacceptable\n" +
              "argument values. The NAME text (string or symbol) identifies the procedure.\n" +
              "The EXPECTED string describes the relevant contract. BAD_POS is the zero-based\n" +
              "position of the bad argument; a negative value means that a specific argument\n" +
              "is not implicated.  The VALUEs are the arguments passed to the procedure.",
              "name", "expected", "bad_pos", "value", DOTDOTDOTPLUS);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(4, args);

        String name     = checkTextArg(0, args);
        String expected = checkStringArg(1, args);
        int    badPos   = checkIntArg(2, args);

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
