// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
        super("test then else",
              "Evaluates the `test` expression first.  If the result is truthy, evaluates the\n" +
              "`then` expression and returns its value.  Otherwise, evaluates the `else`\n" +
              "expression and returns its value.\n" +
              "\n" +
              "All values are \"truthy\" except for false, void, and any variant of null.\n" +
              "\n" +
              "Note that only one of `then` or `else` is evaluated, and both are in tail\n" +
              "position.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        check(expander, stx).arityExact(4);
        return expandArgs(expander, env, stx);
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        CompiledForm testForm = eval.compile(env, stx.get(eval, 1));
        CompiledForm thenForm = eval.compile(env, stx.get(eval, 2));
        CompiledForm elseForm = eval.compile(env, stx.get(eval, 3));

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

            boolean truthy = isTruthy(eval, result).isTrue();

            CompiledForm branch = truthy ? myThenForm : myElseForm;

            return eval.bounceTailForm(store, branch);
        }
    }
}
