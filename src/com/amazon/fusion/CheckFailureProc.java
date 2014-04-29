// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.List;

/**
 * Fusion procedure to raise a unit test failure.
 */
final class CheckFailureProc
    extends Procedure
{
    CheckFailureProc()
    {
        //    "                                                                               |
        super("Raises an exception located at the given STX syntax. The MESSAGEs are\n" +
              "displayed as part of the error.",
              "stack_param", "message", DOTDOTDOT); // FIXME doc above
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

//      String message = safeDisplayManyToString(eval, args, 1);

        DynamicParameter param = (DynamicParameter) args[0];
        List<Object> stack = param.allValues(eval);

        throw new CheckException(stack, "check failure");
    }
}
