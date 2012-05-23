// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonValue;

/**
 *
 */
final class LetrecKeyword
    extends KeywordValue
{
    LetrecKeyword()
    {
        //    "                                                                               |
        super("((IDENT EXPR) ...) BODY ...+",
              "Creates new binding locations for each IDENT, binds them to their EXPRs, then\n" +
              "evaluates BODY. The EXPRs are evaluated left-to-right, and the IDENTs are bound\n" +
              "in all EXPRs and BODYs. BODY may be one or more forms; the result of the last\n" +
              "form is the result of the entire expression.");
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        final int letrecExprSize = expr.size();
        if (letrecExprSize < 3)
        {
            throw new SyntaxFailure(getEffectiveName(), "", expr);
        }

        IonSequence bindingForms =
            requiredSequence("sequence of bindings", 1, expr);

        final int numBindings = bindingForms.size();
        String[]   boundNames = new String[numBindings];
        IonValue[] boundExprs = new IonValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            IonSexp binding =
                requiredSexp("name/value binding", i, bindingForms);
            IonSymbol name = requiredSymbol("name/value binding", 0, binding);
            boundNames[i] = name.stringValue();
            boundExprs[i] = requiredForm("name/value binding", 1, binding);
        }

        FusionValue[] boundValues = new FusionValue[numBindings];
        Environment bodyEnv =
            new LocalEnvironment(env, boundNames, boundValues);

        for (int i = 0; i < numBindings; i++)
        {
            IonValue boundExpr = boundExprs[i];
            FusionValue boundValue = eval.eval(bodyEnv, boundExpr);
            boundValues[i] = boundValue;
        }

        FusionValue result;
        final int bodyEnd = letrecExprSize - 1;
        for (int i = 2; i < bodyEnd; i++)
        {
            IonValue bodyExpr = expr.get(i);
            result = eval.eval(bodyEnv, bodyExpr);
        }

        IonValue bodyExpr = expr.get(bodyEnd);
        result = eval.bounceTailExpression(bodyEnv, bodyExpr);
        return result;
    }
}
