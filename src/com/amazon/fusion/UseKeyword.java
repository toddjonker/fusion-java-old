// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;

/**
 *
 */
final class UseKeyword
    extends KeywordValue
{
    private final EvalFileKeyword myEvalFile;

    UseKeyword(EvalFileKeyword evalFile)
    {
        super("MODULE", "doc");
        myEvalFile = evalFile;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        ModuleInstance module;

        IonValue modStx = expr.get(1);
        if (modStx instanceof IonSymbol)
        {
            IonSymbol name = (IonSymbol) modStx;

            module = (ModuleInstance) eval.eval(env, name);
        }
        else
        {
            String path = ((IonString) modStx).stringValue();
            if (! path.endsWith(".ion")) path += ".ion";

            module = (ModuleInstance) myEvalFile.evalFile(eval, env, path);
        }

        module.useIn((CoreEnvironment) env);

        return UNDEF;
    }
}
