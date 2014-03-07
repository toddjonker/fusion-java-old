// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIterator.iterate;
import static com.amazon.fusion.FusionList.stretchyList;
import static com.amazon.fusion.FusionList.unsafeListAddM;
import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;

final class ForListForm
    extends SyntacticForm
{
    ForListForm()
    {
        //    "                                                                               |
        super("((ident series) ...) body ...+",
              "Iterates the `series` in parallel, binding the corresponding `ident`s to\n" +
              "each element in turn and evaluating `body`.  Each `series` expression must\n" +
              "evaluate to a list, sexp, or iterator.  Returns a stretchy list of the\n" +
              "results.\n" +
              "\n" +
              "If there are no `ident`s declared, then the body is executed once.\n" +
              "\n" +
              "    (for_list\n" +
              "      [ (even [0, 2, 4]),\n" +
              "        (odd  [1, 3, 5]) ]\n" +
              "      (+ even odd))             =>  [1, 5, 9]");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        SyntaxChecker check = check(eval, stx);
        check.arityAtLeast(3);  // Must have bindings and body.

        SyntaxChecker checkBindings =
            check.subformSeq("sequence of bindings", 1);
        SyntaxSequence bindingForms = checkBindings.form();

        final int numBindings = bindingForms.size();
        SyntaxSymbol[] boundNames    = new SyntaxSymbol[numBindings];
        SyntaxValue[]  boundValues   = new SyntaxValue [numBindings];
        SyntaxValue[]  expandedForms = new SyntaxValue [numBindings];
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxChecker checkPair =
                checkBindings.subformSexp("binding pair", i);
            checkPair.arityExact(2);

            boundNames[i] = checkPair.requiredIdentifier("bound name", 0);

            SyntaxValue subform = checkPair.requiredForm("bound value", 1);

            // Bound values use the outer lexical environment
            boundValues[i] = expander.expandExpression(env, subform);
        }

        LocalEnvironment bodyEnv = new LocalEnvironment(env, boundNames, stx);
        SyntaxWrap localWrap = new EnvironmentRenameWrap(bodyEnv);

        // Wrap the bound names so they resolve to their own binding.
        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSymbol name = boundNames[i].addWrap(localWrap);
            name.resolve();
            SourceLocation location = bindingForms.get(eval, i).getLocation();
            expandedForms[i] = SyntaxSexp.make(expander,
                                               location,
                                               name,
                                               boundValues[i]);
        }

        bindingForms = SyntaxSexp.make(expander,
                                       bindingForms.getLocation(),
                                       expandedForms);

        // Prepare the body.
        expandedForms = new SyntaxValue[stx.size()];
        expandedForms[0] = stx.get(eval, 0);
        expandedForms[1] = bindingForms;

        // TODO FUSION-36 Should allow internal definitions
        for (int i = 2; i < stx.size(); i++)
        {
            SyntaxValue bodyStx = stx.get(eval, i);
            bodyStx = bodyStx.addWrap(localWrap);
            expandedForms[i] = expander.expandExpression(bodyEnv, bodyStx);
        }

        stx = SyntaxSexp.make(expander, stx.getLocation(), expandedForms);
        return stx;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp forStx)
        throws FusionException
    {
        SyntaxSequence bindingForms = (SyntaxSequence) forStx.get(eval, 1);

        final int numBindings = bindingForms.size();

        CompiledForm[] valueForms = new CompiledForm[numBindings];

        for (int i = 0; i < numBindings; i++)
        {
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(eval, i);
            SyntaxValue boundExpr = binding.get(eval, 1);
            valueForms[i] = eval.compile(env, boundExpr);
        }

        // Dummy environment to keep track of depth
        env = new LocalEnvironment(env);

        CompiledForm body = BeginForm.compile(eval, env, forStx, 2);

        return new CompiledForList(valueForms, body);
    }


    //========================================================================

    // TODO Optimize for single-series case.
    //      That will make fors_list much more efficient.

    private final class CompiledForList
        implements CompiledForm
    {
        private final CompiledForm[] myValueForms;
        private final CompiledForm   myBody;

        CompiledForList(CompiledForm[] valueForms, CompiledForm body)
        {
            myValueForms = valueForms;
            myBody       = body;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            final int numBindings = myValueForms.length;

            Object resultList = stretchyList(eval, EMPTY_OBJECT_ARRAY);

            if (numBindings != 0)
            {
                FusionIterator[] iters = new FusionIterator[numBindings];

                for (int i = 0; i < numBindings; i++)
                {
                    CompiledForm form = myValueForms[i];
                    Object boundValue = eval.eval(store, form);
                    iters[i] = iterate(eval, boundValue);
                }

                while (FusionIterator.allHaveNext(eval, iters))
                {
                    Object[] boundValues = new Object[numBindings];
                    Store localStore = new LocalStore(store, boundValues);

                    // Determine the next round of bound values
                    for (int i = 0; i < numBindings; i++)
                    {
                        FusionIterator s = iters[i];
                        boundValues[i] = s.next(eval);
                    }

                    Object nextResult = eval.eval(localStore, myBody);
                    unsafeListAddM(eval, resultList, nextResult);
                }
            }
            else // No bindings, evaluate the body once.
            {
                Store localStore = new LocalStore(store, EMPTY_OBJECT_ARRAY);
                Object nextResult = eval.eval(localStore, myBody);
                unsafeListAddM(eval, resultList, nextResult);
            }

            return resultList;
        }
    }
}
