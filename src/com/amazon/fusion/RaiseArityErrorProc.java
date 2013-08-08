// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Arrays;


final class RaiseArityErrorProc
    extends Procedure
{
    RaiseArityErrorProc()
    {
        //    "                                                                               |
        super("Raises a contract failure due to a procedure receiving an unacceptable number\n" +
              "of arguments.\n" +
              "The `name` text (string or symbol) identifies the calling procedure. The\n" +
              "`arity` is an int indicating the expected arity. The `actual_arg`s are the\n" +
              "arguments passed to the procedure.",
              "name", "arity", "actual_arg", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(2, args);

        String name      = checkTextArg(0, args);
        int arity        = checkIntArg(1, args);
        Object[] actuals = Arrays.copyOfRange(args, 2, args.length);

        if (name.isEmpty()) name = "unknown procedure";

        throw new ArityFailure(name, arity, arity, actuals);
    }
}
