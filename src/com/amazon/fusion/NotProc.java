// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class NotProc
    extends Procedure1
{
    NotProc()
    {
        //    "                                                                               |
        super("Returns true if BOOL is false, false if BOOL is true. Throws an exception\n" +
              "otherwise.",
              "bool");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean b = checkBoolArg(0, arg);
        return eval.newBool(! b);
    }
}
