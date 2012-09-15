// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.LocalEnvironment.LexicalBinding;

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
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
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
            boundExpr = boundExpr.expand(eval, bodyEnv);
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
            expandedForms[i] = subform.expand(eval, bodyEnv);
        }

        source = SyntaxSexp.make(source.getLocation(), expandedForms);
        return source;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        SyntaxSexp bindingForms = (SyntaxSexp) expr.get(1);

        final int numBindings = bindingForms.size();

        LexicalBinding[] bindings = new LexicalBinding[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxSymbol boundIdentifier = (SyntaxSymbol) binding.get(0);
            bindings[i] = (LexicalBinding) boundIdentifier.resolve();
        }

        // Dummy environment to keep track of depth
        env = new LocalEnvironment(env, SyntaxSymbol.EMPTY_ARRAY);

        CompiledForm[] valueForms = new CompiledForm[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxValue boundExpr = binding.get(1);
            valueForms[i] = eval.compile(env, boundExpr);
        }

        CompiledForm body = BeginKeyword.compile(eval, env, expr, 2);

        return new CompiledLetrec(bindings, valueForms, body);
    }


    //========================================================================


    private static final class CompiledLetrec
        implements CompiledForm
    {
        private final LexicalBinding[] myBindings;
        private final CompiledForm[]   myValueForms;
        private final CompiledForm     myBody;

        CompiledLetrec(LexicalBinding[] bindings,
                       CompiledForm[]   valueForms,
                       CompiledForm     body)
        {
            myBindings   = bindings;
            myValueForms = valueForms;
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            final int numBindings = myBindings.length;

            FusionValue[] boundValues = new FusionValue[numBindings];
            LocalEnvironment bodyEnv =
                new LocalEnvironment((Environment) store, // TODO remove cast
                                     myBindings, boundValues);

            for (int i = 0; i < numBindings; i++)
            {
                CompiledForm form = myValueForms[i];
                FusionValue boundValue = eval.eval(bodyEnv, form);
                bodyEnv.bind(i, boundValue);
            }

            return eval.bounceTailForm(bodyEnv, myBody);
        }
    }
}
