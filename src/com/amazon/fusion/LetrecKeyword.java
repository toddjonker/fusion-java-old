// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp expr)
        throws SyntaxFailure
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), expr);
        final int letrecExprSize = check.arityAtLeast(3);

        SyntaxSexp bindingForms =
            check.requiredSexp("sequence of bindings", 1);

        final int numBindings = bindingForms.size();
        String[]     boundNames = new String[numBindings];
        SyntaxValue[] boundExprs = new SyntaxValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding =
                requiredSexp("name/value binding", i, bindingForms);
            SyntaxSymbol name = requiredSymbol("name/value binding", 0, binding);
            boundNames[i] = name.stringValue();
            boundExprs[i] = requiredForm("name/value binding", 1, binding);
        }

        FusionValue[] boundValues = new FusionValue[numBindings];
        Environment bodyEnv =
            new LocalEnvironment(env, boundNames, boundValues);

        for (int i = 0; i < numBindings; i++)
        {
            SyntaxValue subform = boundExprs[i];
            SyntaxValue expanded = subform.prepare(eval, bodyEnv);
            if (expanded != subform) boundExprs[i] = expanded;
        }

        for (int i = 2; i < letrecExprSize; i++)
        {
            SyntaxValue subform = expr.get(i);
            SyntaxValue expanded = subform.prepare(eval, bodyEnv);
            if (expanded != subform) boundExprs[i] = expanded;
        }

        return expr;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        SyntaxSexp bindingForms = (SyntaxSexp) expr.get(1);

        final int numBindings = bindingForms.size();
        String[]     boundNames = new String[numBindings];
        SyntaxValue[] boundExprs = new SyntaxValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxSymbol name = (SyntaxSymbol) binding.get(0);
            boundNames[i] = name.stringValue();
            boundExprs[i] = binding.get(1);
        }

        FusionValue[] boundValues = new FusionValue[numBindings];
        Environment bodyEnv =
            new LocalEnvironment(env, boundNames, boundValues);

        for (int i = 0; i < numBindings; i++)
        {
            SyntaxValue boundExpr = boundExprs[i];
            FusionValue boundValue = eval.eval(bodyEnv, boundExpr);
            boundValues[i] = boundValue;
        }

        FusionValue result;
        final int bodyEnd = expr.size() - 1;
        for (int i = 2; i < bodyEnd; i++)
        {
            SyntaxValue bodyExpr = expr.get(i);
            result = eval.eval(bodyEnv, bodyExpr);
        }

        SyntaxValue bodyExpr = expr.get(bodyEnd);
        result = eval.bounceTailExpression(bodyEnv, bodyExpr);
        return result;
    }
}
