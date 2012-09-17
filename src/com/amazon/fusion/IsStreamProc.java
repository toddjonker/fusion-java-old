// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class IsStreamProc
    extends Procedure
{
    IsStreamProc()
    {
        //    "                                                                               |
        super("Checks if the input argument is a stream. Returns true if it is, \n" +
              "false otherwise.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        boolean b = (args[0] instanceof Stream);
        return eval.newBool(b);
    }
}
