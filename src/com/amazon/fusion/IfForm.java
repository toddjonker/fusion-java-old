// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * The {@code if} syntactic form.
 */
final class IfForm
    extends SyntacticForm
{
    IfForm()
    {
        //    "                                                                               |
        super("TEST THEN ELSE",
              "Evaluates the TEST expression first. If the result is truthy, evaluates the THEN\n" +
              "expression and returns its value. Otherwise, evaluates the ELSE expression and\n" +
              "returns its value.\n" +
              "\n" +
              "All values are \"truthy\" except for false, void, and any variant of null.\n" +
              "\n" +
              "Note that only one of THEN or ELSE is evaluated, and both are in tail position.");
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        check(source).arityExact(4);
        return super.expand(eval, env, source);
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        CompiledForm testForm = eval.compile(env, expr.get(1));
        CompiledForm thenForm = eval.compile(env, expr.get(2));
        CompiledForm elseForm = eval.compile(env, expr.get(3));

        return new CompiledIf(testForm, thenForm, elseForm);
    }


    //========================================================================


    private final class CompiledIf
        implements CompiledForm
    {
        private final CompiledForm myTestForm;
        private final CompiledForm myThenForm;
        private final CompiledForm myElseForm;

        CompiledIf(CompiledForm testForm, CompiledForm thenForm,
                   CompiledForm elseForm)
        {
            myTestForm = testForm;
            myThenForm = thenForm;
            myElseForm = elseForm;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object result = eval.eval(store, myTestForm);

            boolean truthy = isTruthy(eval, result);

            CompiledForm branch = truthy ? myThenForm : myElseForm;

            return eval.bounceTailForm(store, branch);
        }
    }
}
