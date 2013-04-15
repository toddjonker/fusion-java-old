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

    GlobalState getGlobalState()
    {
        return myEval.getGlobalState();
    }

    ModuleInstance getKernel()
    {
        return myEval.findKernel();
    }

    boolean isTopLevelContext()
    {
        return myContext == Context.TOP;
    }

    boolean isModuleContext()
    {
        return myContext == Context.MODULE;
    }

    Expander enterModuleContext()
    {
        assert isTopLevelContext();

        return new Expander(myEval, Context.MODULE);
    }


    /**
     * Expands syntax in the current context.
     */
    SyntaxValue expand(Environment env, SyntaxValue stx)
        throws FusionException
    {
        if (stx.getAnnotations().length != 0)
        {
            String message =
                "Annotations not supported in raw syntax. You probably " +
                "want to quote this value";
            throw new SyntaxFailure(null, message, stx);
        }

        return stx.doExpand(this, env);
    }


    /**
     * Expands syntax in an expression context.
     * Equivalent to Racket syntax-local-expand-expression.
     */
    SyntaxValue expandExpression(Environment env, SyntaxValue stx)
        throws FusionException
    {
        Expander expander = this;
        if (myContext != Context.EXPRESSION)
        {
            expander = new Expander(myEval, Context.EXPRESSION);
        }

        return expander.expand(env, stx);
    }


    SyntaxValue expand(Environment env, SyntacticForm form, SyntaxSexp stx)
        throws FusionException
    {
        return form.expand(this, env, stx);
    }
}
