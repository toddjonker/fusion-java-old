// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 * The {@code if} syntactic form.
 */
final class IfKeyword
    extends KeywordValue
{
    IfKeyword()
    {
        super("if", "TEST THEN ELSE",
              "Evaluates the TEST expression; if the result is true, evaluates the THEN\n" +
              "expression, otherwise evaluates the ELSE expression.\n" +
              "Note that only one of THEN or ELSE is evaluated.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        // TODO check number of clauses
        IonValue source = expr.get(1);
        FusionValue result = eval.eval(env, source);
        if (result.isTruthy())
        {
            source = expr.get(2);
        }
        else
        {
            source = expr.get(3);
        }
        result = eval.eval(env, source);
        return result;
    }
}
