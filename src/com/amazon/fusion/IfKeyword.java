// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code if} syntactic form.
 */
final class IfKeyword
    extends KeywordValue
{
    IfKeyword()
    {
        //    "                                                                               |
        super("TEST THEN ELSE",
              "Evaluates the TEST expression first. If the result is true, evaluates the THEN\n" +
              "expression and returns its value. If the result is false, evaluates the ELSE\n" +
              "expression and returns its value.\n" +
              "Note that only one of THEN or ELSE is evaluated, and both are in tail position.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        if (expr.size() != 4)
        {
            throw new SyntaxFailure(getEffectiveName(),
                                    "3 subexpressions required", expr);
        }

        SyntaxValue source = expr.get(1);
        FusionValue result = eval.eval(env, source);

        boolean test = checkBoolArg(0, result);
        int branch = (test ? 2 : 3);
        source = expr.get(branch);

        return eval.bounceTailExpression(env, source);
    }
}
