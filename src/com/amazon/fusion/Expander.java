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
     *
     * @param stx is not enriched before expansion.
     */
    SyntaxValue expand(Environment env, SyntaxValue stx)
        throws FusionException
    {
        if (stx.getAnnotations().length != 0)
        {
            String message =
                "Annotations not supported in raw syntax. You probably " +
                "want to quote this.";
            throw new SyntaxException(null, message, stx);
        }

        return stx.doExpand(this, env);
    }


    /**
     * Partially-expands the syntax until a core syntactic form is exposed.
     * <p>
     * This is close to (or perhaps exactly) Racket's {@code expand-once}.
     *
     * @param stx is not enriched before expansion.
     */
    SyntaxValue partialExpand(Environment env, SyntaxValue stx)
        throws FusionException
    {
        // Handle other cases as per Racket spec. In particular:
        // TODO FUSION-31  identifier macros
        // TODO FUSION-136 rename transformers
        // TODO FUSION-137 #%top
        // TODO FUSION-138 #%app

        while (stx instanceof SyntaxSexp)
        {
            SyntaxSexp sexp = (SyntaxSexp) stx;
            if (sexp.size() == 0) break;

            SyntaxValue first = sexp.get(myEval, 0);
            if (! (first instanceof SyntaxSymbol)) break;

            SyntaxSymbol maybeMacro = (SyntaxSymbol) first;

            SyntacticForm form = maybeMacro.resolveSyntaxMaybe(env);
            if (form == null || ! (form instanceof MacroTransformer))
            {
                // Found a core form or some other kind of binding.

                // TODO FUSION-121 This treats `let` as a core form since
                // LetForm is the only MacroForm that's not a
                // MacroTransformer.  But let expands to procedure call,
                // which no callers of partial expansion care about yet.

                break;
            }

            // We found a static top-level macro binding.
            // Expand it and try again.
            stx = ((MacroTransformer)form).expandOnce(this, sexp);
        }

        return stx;
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
