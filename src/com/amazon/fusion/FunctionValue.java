// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 * Base class for invocable functions, both built-in and user-defined.
 * This implements the evaluation of arguments and prevents the function from
 * access to the caller's environment.
 */
abstract class FunctionValue
    extends FusionValue
{
    @Override
    final FusionValue invoke(Evaluator eval, final Environment env, IonSexp expr)
    {
        IonValue argumentExpr = expr.get(1);
        FusionValue argumentValue = eval.eval(env, argumentExpr);

        return invoke(eval, argumentValue);
    }

    abstract FusionValue invoke(Evaluator eval, FusionValue arg);
}
