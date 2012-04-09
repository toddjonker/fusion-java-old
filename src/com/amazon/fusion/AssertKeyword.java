// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 *
 */
class AssertKeyword
    extends KeywordValue
{
    AssertKeyword()
    {
        super("assert", "EXPR MESSAGE",
              "Evaluates the EXPR, throwing an exception if the result isn't true.\n" +
              "The exception uses the MESSAGE, which is only evaluated on failure.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        IonValue testExpr = expr.get(1);

        FusionValue result = eval.eval(env, testExpr);
        if (IfKeyword.whichBranch(result)) return UNDEF;

        IonValue messageExpr = expr.get(2);
        FusionValue messageValue = eval.eval(env, messageExpr);
        String message = FusionValue.displayToString(messageValue);
        throw new FusionAssertionFailure(message, testExpr, result);
    }
}
