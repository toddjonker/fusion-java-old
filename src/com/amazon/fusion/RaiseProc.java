// Copyright (c) 2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class RaiseProc
    extends Procedure1
{
    // TODO: Add Java API so that embedding code can access raised value even when it's not an exception type.
    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        if (arg instanceof FusionException)
        {
            throw (FusionException) arg;
        }
        throw new FusionUserException("Raised Exception", arg);
    }
}
