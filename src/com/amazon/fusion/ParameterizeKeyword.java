// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class ParameterizeKeyword
    extends KeywordValue
{
    ParameterizeKeyword()
    {
        //    "                                                                               |
        super("((PARAM EXPR) ...) BODY ...+",
              "Dynamically binds the PARAMs to the EXPR values while evaluating the BODY.\n" +
              "The PARAMs are evaluated first, in order; each must result in a dynamic\n" +
              "parameter procedure. The EXPRs are then evaluated in order, and then the params\n" +
              "are changed to their results for the dynamic extent of the BODY.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "entire expression.");
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp expr)
        throws SyntaxFailure
    {
        SyntaxChecker check = new SyntaxChecker(getInferredName(), expr);
        final int exprSize = check.arityAtLeast(3);

        SyntaxSexp bindingForms =
            check.requiredSexp("sequence of parameterizations", 1);

        final int numBindings = bindingForms.size();
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding =
                requiredSexp("parameter/value binding", i, bindingForms);

            SyntaxValue paramExpr =
                requiredForm("parameter/value binding", 0, binding);
            SyntaxValue expanded = paramExpr.prepare(eval, env);
            if (expanded != paramExpr) binding.set(0, expanded);

            SyntaxValue boundExpr =
                requiredForm("parameter/value binding", 1, binding);
            expanded = boundExpr.prepare(eval, env);
            if (expanded != boundExpr) binding.set(1, expanded);
        }

        // Expand the body expressions

        for (int i = 2; i < exprSize; i++)
        {
            SyntaxValue bodyExpr = expr.get(i);
            SyntaxValue expanded = bodyExpr.prepare(eval, env);
            if (expanded != bodyExpr) expr.set(i, expanded);
        }

        return expr;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        SyntaxSexp bindingForms = (SyntaxSexp) expr.get(1);

        final int numBindings = bindingForms.size();
        DynamicParameter[] parameters = new DynamicParameter[numBindings];
        SyntaxValue[] boundExprs = new SyntaxValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxValue paramExpr = binding.get(0);

            FusionValue paramValue = eval.eval(env, paramExpr);
            // TODO error handling
            parameters[i] = (DynamicParameter) paramValue;
            boundExprs[i] = requiredForm("parameter/value binding", 1, binding);
        }

        FusionValue[] boundValues = new FusionValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxValue boundExpr = boundExprs[i];
            FusionValue boundValue = eval.eval(env, boundExpr);
            boundValues[i] = boundValue;
        }

        Evaluator bodyEval = eval.markedContinuation(parameters, boundValues);

        // TODO tail recursion
        FusionValue result = null;
        final int bodyEnd = expr.size() /* - 1 */;
        for (int i = 2; i < bodyEnd; i++)
        {
            SyntaxValue bodyExpr = expr.get(i);
            result = bodyEval.eval(env, bodyExpr);
        }
/*
        IonValue bodyExpr = expr.get(bodyEnd);
        result = eval.bounceTailExpression(env, bodyExpr);
        */
        return result;
    }
}
