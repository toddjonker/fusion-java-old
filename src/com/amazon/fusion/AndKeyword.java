// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 *
 */
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
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        for (int i = 1; i < expr.size(); i++)
        {
            IonValue testExpr = expr.get(i);
            FusionValue v = eval.eval(env, testExpr);
            if (! checkBoolArg(i - 1, v))
            {
                return eval.newBool(false);
            }
        }
        return eval.newBool(true);
    }
}
