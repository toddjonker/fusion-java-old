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
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(source);
        final int letrecExprSize = check.arityAtLeast(3);

        SyntaxChecker checkBindings =
            check.subformSeq("sequence of bindings", 1);
        SyntaxSequence bindingForms = checkBindings.form();

        final int numBindings = bindingForms.size();
        SyntaxSymbol[] boundNames = new SyntaxSymbol[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxChecker checkPair =
                checkBindings.subformSexp("binding pair", i);
            checkPair.arityExact(2);
            boundNames[i] = checkPair.requiredSymbol("bound name", 0);
        }

        Environment bodyEnv = new LocalEnvironment(env, boundNames);
        SyntaxWrap localWrap = new EnvironmentRenameWrap(bodyEnv);

        // Expand the bound-value expressions
        SyntaxValue[] expandedForms = new SyntaxValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            // Wrap the bound names so they resolve to their own binding.
            SyntaxSymbol name = boundNames[i].addWrap(localWrap);
            name.resolve();

            // Already type- and arity-checked this above
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxValue boundExpr = binding.get(1);
            boundExpr = boundExpr.addWrap(localWrap);
            boundExpr = boundExpr.prepare(eval, bodyEnv);
            binding = SyntaxSexp.make(binding.getLocation(),
                                      name,
                                      boundExpr);
            expandedForms[i] = binding;
        }

        bindingForms = SyntaxSexp.make(bindingForms.getLocation(),
                                       expandedForms);

        expandedForms = new SyntaxValue[letrecExprSize];
        expandedForms[0] = source.get(0);
        expandedForms[1] = bindingForms;

        for (int i = 2; i < letrecExprSize; i++)
        {
            SyntaxValue subform = source.get(i);
            subform = subform.addWrap(localWrap);
            expandedForms[i] = subform.prepare(eval, bodyEnv);
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
        SyntaxSymbol[] boundIdentifiers = new SyntaxSymbol[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            boundIdentifiers[i] = (SyntaxSymbol) binding.get(0);
        }

        FusionValue[] boundValues = new FusionValue[numBindings];
        LocalEnvironment bodyEnv =
            new LocalEnvironment(env, boundIdentifiers, boundValues);

        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxValue boundExpr = binding.get(1);
            FusionValue boundValue = eval.eval(bodyEnv, boundExpr);
            bodyEnv.bind(i, boundValue);
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
