// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class AndKeyword
    extends KeywordValue
{
    AndKeyword()
    {
        //    "                                                                               |
        super("EXPR ...",
              "Evaluates the EXPRs from left to right, returning true if they all return true,\n" +
              "false if any return false. An exception is raised any EXPR evaluates to a\n" +
              "non-boolean value.");
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        // Cannot optimize single-form scenario, because we need to type-check
        // the result.

        CompiledForm[] forms = eval.compile(env, source, 1);
        return new CompiledAnd(forms);
    }


    //========================================================================


    private final class CompiledAnd
        implements CompiledForm
    {
        private final CompiledForm[] myForms;

        CompiledAnd(CompiledForm[] forms)
        {
            myForms = forms;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            for (int i = 0; i < myForms.length; i++)
            {
                CompiledForm form = myForms[i];
                Object v = eval.eval(store, form);
                if (! checkBoolArg(i, v))
                {
                    return eval.newBool(false);
                }
            }
            return eval.newBool(true);
        }
    }
}
