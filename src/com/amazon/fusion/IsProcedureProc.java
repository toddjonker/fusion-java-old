// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class IsProcedureProc
    extends Procedure1
{
    IsProcedureProc()
    {
        //    "                                                                               |
        super("Returns `true` when `value` is a procedure, `false` otherwise.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        return eval.newBool(arg instanceof Procedure);
    }
}
