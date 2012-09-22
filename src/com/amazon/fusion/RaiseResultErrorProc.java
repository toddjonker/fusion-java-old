// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;



final class RaiseResultErrorProc
    extends Procedure
{
    RaiseResultErrorProc()
    {
        //    "                                                                               |
        super("Raises a contract failure due to a procedure returning an unacceptable value.\n" +
              "The NAME text (string or symbol) identifies the calling procedure. The\n" +
              "EXPECTED string describes the relevant contract. The VALUE is the faulty\n" +
              "result.",
              "name", "expected", "value");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(3, args);

        String name     = checkTextArg(0, args);
        String expected = checkStringArg(1, args);
        Object actual = args[2];

        if (name.isEmpty()) name = "unknown procedure";

        throw new ResultFailure(name, expected, actual);
    }
}
