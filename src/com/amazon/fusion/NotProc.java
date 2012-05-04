// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
class NotProc
    extends Procedure
{
    NotProc()
    {
        //    "                                                                               |
        super("Returns true if BOOL is false, false if BOOL is true. Throws an exception\n" +
              "otherwise.",
              "BOOL");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        boolean b = checkBoolArg(0, args);
        return eval.newBool(! b);
    }

}
