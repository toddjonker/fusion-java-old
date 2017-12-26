// Copyright (c) 2014-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.List;

/**
 * Fusion procedure to raise a unit test failure.
 */
final class FailCheckProc
    extends Procedure0
{
    private final DynamicParameter myCheckStackParam;

    public FailCheckProc(Object stackParam)
    {
        myCheckStackParam = (DynamicParameter) stackParam;
    }


    @Override
    Object doApply(Evaluator eval)
        throws FusionException
    {
        List<Object> stack = myCheckStackParam.allValues(eval);

        throw new CheckException(stack, "check failure");
    }
}
