// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class AssertKeyword
    extends KeywordValue
{
    AssertKeyword()
    {
        //    "                                                                               |
        super("EXPR MESSAGE ...",
              "Evaluates the EXPR, throwing an exception if the result isn't true.\n" +
              "The exception displays the MESSAGEs, which are only evaluated on failure.");
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        check(source).arityAtLeast(2);
        return super.prepare(eval, env, source);
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        SyntaxValue testExpr = expr.get(1);
        FusionValue result = eval.eval(env, testExpr);
        if (checkBoolArg(0 /* argNum */, result)) return UNDEF;

        String message;
        int size = expr.size();
        if (size > 2)
        {
            StringBuilder buf = new StringBuilder();
            for (int i = 2; i < size; i++)
            {
                SyntaxValue messageExpr = expr.get(i);
                FusionValue messageValue = eval.eval(env, messageExpr);
                FusionValue.display(buf, messageValue);
            }
            message = buf.toString();
        }
        else
        {
            message = null;
        }

        throw new FusionAssertionFailure(message, testExpr, result);
    }
}
