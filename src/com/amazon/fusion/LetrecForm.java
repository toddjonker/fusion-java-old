// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Arrays;

final class LetrecForm
    extends SyntacticForm
{
    LetrecForm()
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
        throws FusionException
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
            boundNames[i] = checkPair.requiredIdentifier("bound name", 0);
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
        // Dummy environment to keep track of depth
        env = new LocalEnvironment(env, SyntaxSymbol.EMPTY_ARRAY);

        SyntaxSexp bindingForms = (SyntaxSexp) expr.get(1);

        final int numBindings = bindingForms.size();

        CompiledForm[] valueForms = new CompiledForm[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxValue boundExpr = binding.get(1);
            valueForms[i] = eval.compile(env, boundExpr);
        }

        CompiledForm body = BeginForm.compile(eval, env, expr, 2);

        switch (valueForms.length)
        {
            case 1:
                return new CompiledLetrec1(valueForms, body);
            case 2:
                return new CompiledLetrec2(valueForms, body);
            default:
                return new CompiledLetrec(valueForms, body);
        }
    }


    //========================================================================


    private static final class CompiledLetrec
        implements CompiledForm
    {
        private final CompiledForm[] myValueForms;
        private final CompiledForm   myBody;

        CompiledLetrec(CompiledForm[] valueForms, CompiledForm body)
        {
            myValueForms = valueForms;
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            final int numBindings = myValueForms.length;

            Object[] boundValues = new Object[numBindings];
            Arrays.fill(boundValues, UNDEF);

            Store localStore = new LocalStore(store, boundValues);

            for (int i = 0; i < numBindings; i++)
            {
                CompiledForm form = myValueForms[i];
                boundValues[i] = eval.eval(localStore, form);
            }

            return eval.bounceTailForm(localStore, myBody);
        }
    }


    private static final class CompiledLetrec1
        implements CompiledForm
    {
        private final CompiledForm myValueForm0;
        private final CompiledForm myBody;

        CompiledLetrec1(CompiledForm[] valueForms, CompiledForm body)
        {
            myValueForm0 = valueForms[0];
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Store localStore = new LocalStore1(store, UNDEF);

            Object value = eval.eval(localStore, myValueForm0);
            localStore.set(0, value);

            return eval.bounceTailForm(localStore, myBody);
        }
    }


    private static final class CompiledLetrec2
        implements CompiledForm
    {
        private final CompiledForm myValueForm0;
        private final CompiledForm myValueForm1;
        private final CompiledForm myBody;

        CompiledLetrec2(CompiledForm[] valueForms, CompiledForm body)
        {
            myValueForm0 = valueForms[0];
            myValueForm1 = valueForms[1];
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Store localStore = new LocalStore2(store, UNDEF, UNDEF);

            Object value = eval.eval(localStore, myValueForm0);
            localStore.set(0, value);

            value = eval.eval(localStore, myValueForm1);
            localStore.set(1, value);

            return eval.bounceTailForm(localStore, myBody);
        }
    }
}
