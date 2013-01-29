// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.FusionWrite.safeDisplay;

final class AssertForm
    extends SyntacticForm
{
    AssertForm()
    {
        //    "                                                                               |
        super("EXPR MESSAGE ...",
              "Evaluates the EXPR, throwing an exception if the result isn't truthy (as\n" +
              "defined by `if`.\n" +
              "The exception displays the MESSAGEs, which are only evaluated on failure.");
    }


    @Override
    SyntaxValue expand(Evaluator eval, ExpandContext ctx, Environment env, SyntaxSexp source)
        throws FusionException
    {
        check(source).arityAtLeast(2);
        return super.expand(eval, ctx, env, source);
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxValue testFormSyntax = source.get(1);
        CompiledForm testForm = eval.compile(env, testFormSyntax);

        CompiledForm[] messageForms = eval.compile(env, source, 2);
        return new CompiledAssert(testFormSyntax, testForm, messageForms);
    }


    //========================================================================


    private final class CompiledAssert
        implements CompiledForm
    {
        private final SyntaxValue    myTestFormSyntax; // For error reporting
        private final CompiledForm   myTestForm;
        private final CompiledForm[] myMessageForms;

        CompiledAssert(SyntaxValue testFormSyntax, CompiledForm testForm,
                       CompiledForm[] messageForms)
        {
            myTestFormSyntax = testFormSyntax;
            myTestForm       = testForm;
            myMessageForms   = messageForms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object result = eval.eval(store, myTestForm);
            if (isTruthy(eval, result))
            {
                return voidValue(eval);
            }

            String message;
            int size = myMessageForms.length;
            if (size != 0)
            {
                StringBuilder buf = new StringBuilder();
                for (CompiledForm messageForm : myMessageForms)
                {
                    Object messageValue = eval.eval(store, messageForm);

                    // Use safe API so we don't throw a different exception
                    safeDisplay(eval, buf, messageValue);
                }
                message = buf.toString();
            }
            else
            {
                message = null;
            }

            throw new FusionAssertionFailure(message, myTestFormSyntax, result);
        }
    }
}
