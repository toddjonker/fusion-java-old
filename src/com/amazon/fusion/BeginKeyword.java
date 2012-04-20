// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 * The {@code begin} syntactic form.
 */
final class BeginKeyword
    extends KeywordValue
{
    BeginKeyword()
    {
        //    "                                                                               |
        super("EXPR ...",
              "Evaluates the EXPRs in order, returning the final result.\n" +
              "The last EXPR is in tail position.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        FusionValue result = null;
        final int last = expr.size() - 1;
        for (int i = 1; i < last; i++)
        {
            IonValue source = expr.get(i);
            result = eval.eval(env, source);
        }

        IonValue source = expr.get(last);
        result = eval.bounceTailExpression(env, source);
        return result;
    }
}
