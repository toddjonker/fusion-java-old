// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class OrKeyword
    extends KeywordValue
{
    OrKeyword()
    {
        //    "                                                                               |
        super("EXPR ...",
              "Evaluates the EXPRs from left to right, returning false if they all return true,\n" +
              "true if any return true. An exception is raised any EXPR evaluates to a\n" +
              "non-boolean value.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        // TODO FUSION-12 tail optimization

        for (int i = 1; i < expr.size(); i++)
        {
            SyntaxValue testExpr = expr.get(i);
            FusionValue v = eval.eval(env, testExpr);
            if (checkBoolArg(i - 1, v))
            {
                return eval.newBool(true);
            }
        }
        return eval.newBool(false);
    }
}
