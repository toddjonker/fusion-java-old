// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class NotProc
    extends Procedure1
{
    NotProc()
    {
        //    "                                                                               |
        super("Returns true if VALUE is untruthy, false if VALUE is truthy. Truthiness is as\n" +
              "defined by `if`.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean b = isTruthy(eval, arg);
        return eval.newBool(! b);
    }
}
