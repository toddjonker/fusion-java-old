// Copyright (c) 2012-2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Throws an {@link ExitException}, which will generally cause the runtime
 * to exit its evaluation.
 */
final class ExitProc
    extends Procedure0
{
    @Override
    Object doApply(Evaluator eval)
        throws ExitException
    {
        throw new ExitException();
    }
}
