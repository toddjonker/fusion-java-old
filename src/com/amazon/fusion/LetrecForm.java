// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Arrays;

final class LetrecForm
    extends SyntacticForm
{
    LetrecForm()
    {
        //    "                                                                               |
        super("((ident expr) ...) body ...+",
              "Creates new binding locations for each `ident`, binds them to their `expr`s,\n"
            + "then evaluates `body`. The `expr`s are evaluated left-to-right, and the\n"
            + "`ident`s are bound in all `expr`s and `body`s. `body` may be one or more forms;\n"
            + "the result of the last form is the result of the entire expression.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        SyntaxChecker check = check(eval, stx);
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

        Environment bodyEnv = new LocalEnvironment(env, boundNames, stx);
        SyntaxWrap localWrap = new EnvironmentRenameWrap(bodyEnv);

        // Expand the bound-value expressions
        SyntaxValue[] expandedForms = new SyntaxValue[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            // Wrap the bound names so they resolve to their own binding.
            SyntaxSymbol name = boundNames[i].addWrap(localWrap);
            name.resolve();

            // Already type- and arity-checked this above
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(eval, i);
            SyntaxValue boundExpr = binding.get(eval, 1);
            boundExpr = boundExpr.addWrap(localWrap);
            boundExpr = expander.expandExpression(bodyEnv, boundExpr);
            binding = SyntaxSexp.make(eval, binding.getLocation(),
                                      name,
                                      boundExpr);
            expandedForms[i] = binding;
        }

        bindingForms = SyntaxSexp.make(expander, bindingForms.getLocation(),
                                       expandedForms);

        expandedForms = new SyntaxValue[letrecExprSize];
        expandedForms[0] = stx.get(eval, 0);
        expandedForms[1] = bindingForms;

        // TODO FUSION-36 Should allow internal definitions
        for (int i = 2; i < letrecExprSize; i++)
        {
            SyntaxValue subform = stx.get(eval, i);
            subform = subform.addWrap(localWrap);
            expandedForms[i] = expander.expandExpression(bodyEnv, subform);
        }

        stx = SyntaxSexp.make(eval, stx.getLocation(), expandedForms);
        return stx;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        // Dummy environment to keep track of depth
        env = new LocalEnvironment(env);

        SyntaxSexp bindingForms = (SyntaxSexp) stx.get(eval, 1);

        final int numBindings = bindingForms.size();

        CompiledForm[] valueForms = new CompiledForm[numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(eval, i);
            SyntaxValue boundExpr = binding.get(eval, 1);
            valueForms[i] = eval.compile(env, boundExpr);
        }

        CompiledForm body = BeginForm.compile(eval, env, stx, 2);

        switch (valueForms.length)
        {
            case 0:
                return body;
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
