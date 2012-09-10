// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class OrKeyword
    extends KeywordValue
{
    OrKeyword()
    {
        //    "                                                                               |
        super("EXPR ...",
              "Evaluates the EXPRs from left to right, returning false if they all return true,\n" +
              "true if any return true. An exception is raised any EXPR evaluates to a\n" +
              "non-boolean value.");
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        // Cannot optimize single-form scenario, because we need to type-check
        // the result.

        CompiledForm[] forms = eval.compile(env, source, 1);
        return new CompiledOr(forms);
    }


    //========================================================================


    private final class CompiledOr
        implements CompiledForm
    {
        private final CompiledForm[] myForms;

        CompiledOr(CompiledForm[] forms)
        {
            myForms = forms;
        }

        @Override
        public Object doExec(Evaluator eval, Store store)
            throws FusionException
        {
            for (int i = 0; i < myForms.length; i++)
            {
                CompiledForm form = myForms[i];
                Object v = eval.exec(store, form);
                if (checkBoolArg(i, v))
                {
                    return eval.newBool(true);
                }
            }
            return eval.newBool(false);
        }
    }
}
