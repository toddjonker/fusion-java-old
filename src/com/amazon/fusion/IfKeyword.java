// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code if} syntactic form.
 */
final class IfKeyword
    extends KeywordValue
{
    IfKeyword()
    {
        //    "                                                                               |
        super("TEST THEN ELSE",
              "Evaluates the TEST expression first. If the result is true, evaluates the THEN\n" +
              "expression and returns its value. If the result is false, evaluates the ELSE\n" +
              "expression and returns its value.\n" +
              "Note that only one of THEN or ELSE is evaluated, and both are in tail position.");
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
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

            boolean test = checkBoolArg(0, result);
            CompiledForm branch = (test ? myThenForm : myElseForm);

            return eval.bounceTailForm(store, branch);
        }
    }
}
