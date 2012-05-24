// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonString;
import com.amazon.ion.IonValue;

/**
 * This is syntax because we currently evaluate the file in the invoker's
 * environment. That's not right but there's no access on anything else now.
 */
final class EvalFileKeyword
    extends KeywordValue
{
    private final LoadHandler myLoadHandler;

    EvalFileKeyword(LoadHandler loadHandler)
    {
        //    "                                                                               |
        super("FILENAME",
              "Opens the Fusion source file named by the given string and evaluates each\n" +
              "expression in sequence. Returns the last result.\n" +
              "FILENAME is resolve relative to the value of current_directory.");

        myLoadHandler = loadHandler;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        String fileName;
        {
            IonValue argExpr = expr.get(1);
            FusionValue argValue = eval.eval(env, argExpr);
            IonString nameDom = (IonString) ((DomValue) argValue).ionValue();
            fileName = nameDom.stringValue();
        }

        Namespace namespace = env.namespace();
        return myLoadHandler.loadTopLevel(eval, namespace, fileName);
    }
}
