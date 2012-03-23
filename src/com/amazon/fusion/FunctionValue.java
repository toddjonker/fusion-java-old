// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;


abstract class FunctionValue
    extends FusionValue
{
    @Override
    FusionValue invoke(Evaluator eval, final Environment env, IonSexp expr)
    {
        IonValue argumentExpr = expr.get(1);
        FusionValue argumentValue = eval.eval(env, argumentExpr);

        return invoke(eval, argumentValue);
    }

    abstract FusionValue invoke(Evaluator eval, FusionValue arg);
}
