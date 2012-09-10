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
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(source);
        final int exprSize = check.arityAtLeast(3);

        SyntaxChecker checkBindings =
            check.subformSeq("sequence of parameterizations", 1);
        SyntaxSequence bindingForms = checkBindings.form();

        final int numBindings = bindingForms.size();
        SyntaxValue[] expandedForms = new SyntaxValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxChecker checkPair =
                checkBindings.subformSexp("parameter/value pair", i);
            checkPair.arityExact(2);

            SyntaxSexp binding = (SyntaxSexp) checkPair.form();

            SyntaxValue paramExpr = binding.get(0);
            paramExpr = paramExpr.prepare(eval, env);

            SyntaxValue boundExpr = binding.get(1);
            boundExpr = boundExpr.prepare(eval, env);

            binding = SyntaxSexp.make(binding.getLocation(),
                                      paramExpr, boundExpr);
            expandedForms[i] = binding;
        }

        bindingForms = SyntaxSexp.make(bindingForms.getLocation(),
                                       expandedForms);

        // Expand the body expressions
        expandedForms = new SyntaxValue[exprSize];
        expandedForms[0] = source.get(0);
        expandedForms[1] = bindingForms;

        for (int i = 2; i < exprSize; i++)
        {
            SyntaxValue bodyExpr = source.get(i);
            expandedForms[i] = bodyExpr.prepare(eval, env);
        }

        source = SyntaxSexp.make(source.getLocation(), expandedForms);
        return source;
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
            try
            {
                parameters[i] = (DynamicParameter) paramValue;
            }
            catch (ClassCastException e)
            {
                String message =
                    "Parameter expression evaluated to non-parameter: " +
                    writeToString(paramValue);
                throw contractFailure(message);
            }
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
