// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;
import static com.amazon.fusion.FusionText.checkRequiredTextArg;
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
              "arguments passed to the procedure.\n" +
              "\n" +
              "Note that an arity exception IS-A contract exception.",
              "name", "arity", "actual_arg", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(2, args);

        String name      = checkRequiredTextArg(eval, this, 0, args);
        int arity        = checkIntArgToJavaInt(eval, this, 1, args);
        Object[] actuals = Arrays.copyOfRange(args, 2, args.length);

        if (name.isEmpty()) name = "unknown procedure";

        throw new ArityFailure(name, arity, arity, actuals);
    }
}
