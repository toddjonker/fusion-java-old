// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 *
 */
final class AssertKeyword
    extends KeywordValue
{
    AssertKeyword()
    {
        //    "                                                                               |
        super("EXPR MESSAGE",
              "Evaluates the EXPR, throwing an exception if the result isn't true.\n" +
              "The exception displays the MESSAGE, which is only evaluated on failure.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        int size = expr.size();
        if (size < 2)
        {
            throw new SyntaxFailure(getEffectiveName(),
                                    "test-expression required",
                                    expr);
        }

        IonValue testExpr = expr.get(1);
        FusionValue result = eval.eval(env, testExpr);
        if (checkBoolArg(0 /* argNum */, result)) return UNDEF;

        IonValue messageExpr = expr.get(2);
        FusionValue messageValue = eval.eval(env, messageExpr);
        String message = messageValue.display();
        throw new FusionAssertionFailure(message, testExpr, result);
    }
}
