// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;

/**
 *
 */
final class ModuleKeyword
    extends KeywordValue
{
    ModuleKeyword()
    {
        super("???", "doc");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        Environment moduleEnv = new CoreEnvironment(eval);

        for (int i = 1; i < expr.size(); i++)
        {
            eval.eval(moduleEnv, expr.get(i));
        }

        return new ModuleInstance(eval, moduleEnv);
    }
}
