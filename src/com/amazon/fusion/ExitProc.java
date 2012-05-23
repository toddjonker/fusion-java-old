// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Throws an {@link ExitException}, which will generally cause the runtime
 * to exit its evaluation.
 */
final class ExitProc
    extends Procedure
{
    ExitProc()
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
