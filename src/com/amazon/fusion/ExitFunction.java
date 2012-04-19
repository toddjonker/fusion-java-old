// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Language.ExitException;

/**
 *
 */
class ExitFunction
    extends FunctionValue
{
    ExitFunction()
    {
        //    "                                                                               |
        super("Exits the running program.");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] arg)
        throws ExitException
    {
        throw new ExitException();
    }
}
