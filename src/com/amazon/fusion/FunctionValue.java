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
        throws FusionException
    {
        int argCount = expr.size() - 1;

        FusionValue[] args;
        if (argCount == 0)
        {
            args = FusionValue.EMPTY_ARRAY;
        }
        else
        {
            args = new FusionValue[argCount];
            for (int i = 0; i < argCount; i++)
            {
                IonValue argumentExpr = expr.get(i + 1);
                FusionValue argumentValue = eval.eval(env, argumentExpr);
                args[i] = argumentValue;
            }
        }

        return invoke(eval, args);
    }

    /**
     * @param args must not be null, and none of its elements may be null.
     * @return null is a synonym for {@link #UNDEF}.
     */
    abstract FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException;
}
