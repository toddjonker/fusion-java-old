// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

class AndKeyword
    extends KeywordValue
{
    AndKeyword()
    {
        //    "                                                                               |
        super("EXPR ...",
              "Evaluates the EXPRs from left to right, returning true if they all return true,\n" +
              "false if any return false. An exception is raised any EXPR evaluates to a\n" +
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
            if (! checkBoolArg(i - 1, v))
            {
                return eval.newBool(false);
            }
        }
        return eval.newBool(true);
    }
}
