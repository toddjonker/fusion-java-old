// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class NotProc
    extends Procedure1
{
    NotProc()
    {
        //    "                                                                               |
        super("Returns `true` if `value` is untruthy, `false` if `value` is truthy. Truthiness\n"
            + "is as defined by `if`.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        return not(eval, arg);
    }
}
