// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;

/**
 * The {@code if} syntactic form.
 */
final class IfKeyword
    extends KeywordValue
{
    /**
     * Returns true or false indicating which branch an {@code if} expression
     * would take.
     *
     * @throws FusionException if the value can't be used as an if-test
     * expression.
     */
    static boolean whichBranch(NamedValue form, int argNum, FusionValue fv)
        throws FusionException
    {
        if (fv instanceof DomValue)
        {
            IonValue iv = ((DomValue) fv).getDom();
            if (iv.getType() == IonType.BOOL && ! iv.isNullValue())
            {
                return ((IonBool) iv).booleanValue();
            }
        }

        throw new ArgTypeFailure(form, "true or false", argNum, fv);
    }


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
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        if (expr.size() != 4)
        {
            throw new SyntaxFailure(getEffectiveName(),
                                    "3 subexpressions required", expr);
        }

        IonValue source = expr.get(1);
        FusionValue result = eval.eval(env, source);

        int branch = whichBranch(this, 1, result) ? 2 : 3;
        source = expr.get(branch);

        return eval.bounceTailExpression(env, source);
    }
}
