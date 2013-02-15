// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;

final class LetValuesForm
    extends SyntacticForm
{
    LetValuesForm()
    {
        //    "                                                                               |
        super("(((IDENT ...) EXPR) ...) BODY ...+",
              "Creates local bindings for the IDENTs, with the BODY in scope.  The EXPRs are\n" +
              "evaluated left-to-right, and must return as many values as there are\n" +
              "corresponding IDENTs, which are then bound to those results.  After the\n" +
              "bindings are installed the BODY is evaluated.  BODY may be one or more forms;\n" +
              "the last form is in tail position and its result is the result of the entire\n" +
              "expression.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxChecker check = check(stx);
        final int letExprSize = check.arityAtLeast(3);

        SyntaxChecker checkBindings =
            check.subformSeq("sequence of bindings", 1);
        SyntaxSequence bindingForms = checkBindings.form();

        final int numBindingForms = bindingForms.size();

        // Gather the bound names
        ArrayList<SyntaxSymbol> boundNameList =
            new ArrayList<SyntaxSymbol>(numBindingForms);
        for (int i = 0; i < numBindingForms; i++)
        {
            SyntaxChecker checkPair =
                checkBindings.subformSexp("binding pair", i);
            checkPair.arityExact(2);

            SyntaxChecker checkBoundNames =
                checkPair.subformSexp("bound names sequence", 0);

            for (int j = 0; j < checkBoundNames.form().size(); j++)
            {
                SyntaxSymbol name =
                    checkBoundNames.requiredIdentifier("bound name", j);
                boundNameList.add(name);
            }
        }

        SyntaxSymbol[] boundNames =
            boundNameList.toArray(new SyntaxSymbol[boundNameList.size()]);
        Environment bodyEnv = new LocalEnvironment(env, boundNames);
        SyntaxWrap localWrap = new EnvironmentRenameWrap(bodyEnv);

        // Expand the bound-value expressions
        SyntaxValue[] expandedForms = new SyntaxValue[numBindingForms];
        int bindingPos = 0;
        for (int i = 0; i < numBindingForms; i++)
        {
            // Already type- and arity-checked this above
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);

            SyntaxSexp names = (SyntaxSexp) binding.get(0);
            SyntaxValue[] wrappedNames = names.extract();
            for (int j = 0; j < names.size(); j++)
            {
                // Wrap the bound names so they resolve to their own binding.
                SyntaxSymbol name = boundNames[bindingPos];
                assert name == wrappedNames[j];

                name = name.addWrap(localWrap);
                name.resolve();
                wrappedNames[j] = name;
                bindingPos++;
            }
            names = SyntaxSexp.make(expander, names.getLocation(),
                                    wrappedNames);

            SyntaxValue boundExpr = binding.get(1);
            boundExpr = expander.expandExpression(env, boundExpr);
            binding = SyntaxSexp.make(expander, binding.getLocation(),
                                      names,
                                      boundExpr);
            expandedForms[i] = binding;
        }
        assert bindingPos == boundNames.length;

        bindingForms = SyntaxSexp.make(expander, bindingForms.getLocation(),
                                       expandedForms);

        expandedForms = new SyntaxValue[letExprSize];
        expandedForms[0] = stx.get(0);
        expandedForms[1] = bindingForms;

        // TODO FUSION-36 Should allow internal definitions
        for (int i = 2; i < letExprSize; i++)
        {
            SyntaxValue subform = stx.get(i);
            subform = subform.addWrap(localWrap);
            expandedForms[i] = expander.expandExpression(bodyEnv, subform);
        }

        stx = SyntaxSexp.make(expander, stx.getLocation(), expandedForms);
        return stx;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        SyntaxSexp bindingForms = (SyntaxSexp) expr.get(1);

        final int numBindingForms = bindingForms.size();

        int[] valueCounts = new int[numBindingForms];
        CompiledForm[] valueForms = new CompiledForm[numBindingForms];

        int bindingCount = 0;
        boolean allSingles = true;
        for (int i = 0; i < numBindingForms; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);

            SyntaxSexp names = (SyntaxSexp) binding.get(0);
            int size = names.size();
            bindingCount += size;
            valueCounts[i] = size;

            allSingles &= (size == 1);

            SyntaxValue boundExpr = binding.get(1);
            valueForms[i] = eval.compile(env, boundExpr);
        }

        // Dummy environment to keep track of depth
        env = new LocalEnvironment(env, SyntaxSymbol.EMPTY_ARRAY);
        CompiledForm body = BeginForm.compile(eval, env, expr, 2);

        if (allSingles)
        {
            return compilePlainLet(valueForms, body);
        }

        return new CompiledLetValues(bindingCount, valueCounts, valueForms,
                                     body);
    }


    static CompiledForm compilePlainLet(CompiledForm[] valueForms,
                                        CompiledForm body)
    {
        switch (valueForms.length)
        {
            case 0:
                return body;
            case 1:
                return new CompiledPlainLet1(valueForms, body);
            case 2:
                return new CompiledPlainLet2(valueForms, body);
            default:
                return new CompiledPlainLet(valueForms, body);
        }
    }


    //========================================================================


    private static final class CompiledPlainLet
        implements CompiledForm
    {
        private final CompiledForm[] myValueForms;
        private final CompiledForm   myBody;

        CompiledPlainLet(CompiledForm[] valueForms, CompiledForm body)
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

            for (int i = 0; i < numBindings; i++)
            {
                CompiledForm form = myValueForms[i];
                Object values = eval.eval(store, form);
                checkSingleResult(values);
                boundValues[i] = values;
            }

            Store localStore = new LocalStore(store, boundValues);
            return eval.bounceTailForm(localStore, myBody);
        }
    }


    private static final class CompiledPlainLet1
        implements CompiledForm
    {
        private final CompiledForm myValueForm0;
        private final CompiledForm myBody;

        CompiledPlainLet1(CompiledForm[] valueForms, CompiledForm body)
        {
            myValueForm0 = valueForms[0];
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object value = eval.eval(store, myValueForm0);
            checkSingleResult(value);

            Store localStore = new LocalStore1(store, value);
            return eval.bounceTailForm(localStore, myBody);
        }
    }


    private static final class CompiledPlainLet2
        implements CompiledForm
    {
        private final CompiledForm myValueForm0;
        private final CompiledForm myValueForm1;
        private final CompiledForm myBody;

        CompiledPlainLet2(CompiledForm[] valueForms, CompiledForm body)
        {
            myValueForm0 = valueForms[0];
            myValueForm1 = valueForms[1];
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object value0 = eval.eval(store, myValueForm0);
            checkSingleResult(value0);

            Object value1 = eval.eval(store, myValueForm1);
            checkSingleResult(value1);

            Store localStore = new LocalStore2(store, value0, value1);
            return eval.bounceTailForm(localStore, myBody);
        }
    }


    //========================================================================


    private static final class CompiledLetValues
        implements CompiledForm
    {
        private final int            myBindingCount;
        private final int[]          myValueCounts;
        private final CompiledForm[] myValueForms;
        private final CompiledForm   myBody;

        CompiledLetValues(int bindingCount, int[] valueCounts,
                          CompiledForm[] valueForms, CompiledForm body)
        {
            assert valueCounts.length == valueForms.length;

            myBindingCount = bindingCount;
            myValueCounts = valueCounts;
            myValueForms = valueForms;
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            final int numBindings = myValueForms.length;

            Object[] boundValues = new Object[myBindingCount];

            int bindingPos = 0;
            for (int i = 0; i < numBindings; i++)
            {
                CompiledForm form = myValueForms[i];
                Object values = eval.eval(store, form);

                int expectedCount = myValueCounts[i];
                if (expectedCount == 1)
                {
                    checkSingleResult(values);
                    boundValues[bindingPos++] = values;
                }
                else if (values instanceof Object[])
                {
                    String message = "Can't bind multiple values";
                    throw new UnsupportedOperationException(message);
                }
                else
                {
                    String expectation =
                        expectedCount + " results but received 1";
                    throw new ResultFailure("local-binding form",
                                            expectation, -1, values);
                }
            }

            Store localStore = new LocalStore(store, boundValues);
            return eval.bounceTailForm(localStore, myBody);
        }
    }

    private static final void checkSingleResult(Object values)
        throws FusionException
    {
        if (values instanceof Object[])
        {
            Object[] valuesArray = (Object[]) values;
            assert valuesArray.length > 1;
            String expectation =
                "1 result but received " + valuesArray.length;
            throw new ResultFailure("local-binding form",
                                    expectation, -1, valuesArray);
        }
    }
}
