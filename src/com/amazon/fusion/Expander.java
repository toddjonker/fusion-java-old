// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * "Registers" used during macro expansion
 */
final class Expander
{
    enum Context { TOP, MODULE, EXPRESSION }

    private final Evaluator myEval;
    private final Context   myContext;

    Expander(Evaluator eval)
    {
        myEval = eval;
        myContext = Context.TOP;
    }

    private Expander(Evaluator eval, Context ctx)
    {
        myEval = eval;
        myContext = ctx;
    }


    Evaluator getEvaluator()
    {
        return myEval;
    }

    ModuleInstance getKernel()
    {
        return myEval.findKernel();
    }

    boolean isTopLevel()
    {
        return myContext == Context.TOP;
    }

    Expander nestModule()
    {
        assert isTopLevel();

        return new Expander(myEval, Context.MODULE);
    }


    SyntaxValue expand(Environment env, SyntaxValue stx)
        throws FusionException
    {
        // TODO FUSION-43 Fail if there are annotations on stx
        return stx.doExpand(this, env);
    }


    SyntaxValue expand(Environment env, SyntacticForm form, SyntaxSexp stx)
        throws FusionException
    {
        return form.expand(myEval, this, env, stx);
    }
}
