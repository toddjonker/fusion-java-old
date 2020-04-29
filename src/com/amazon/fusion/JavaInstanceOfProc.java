// Copyright (c) 2017-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;

final class JavaInstanceOfProc
    extends Procedure2
{
    @Override
    Object doApply(Evaluator eval, Object arg0, Object arg1)
        throws FusionException
    {
        Class<?> klass = checkArg(Class.class, "arg", 0, arg0);

        return makeBool(eval, klass.isInstance(arg1));
    }
}
