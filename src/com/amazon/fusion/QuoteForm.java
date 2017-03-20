// Copyright (c) 2012-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code quote} syntactic form.
 */
final class QuoteForm
    extends SyntacticForm
{
    QuoteForm()
    {
        //    "                                                                               |
        super("datum",
              "Returns the Ion `datum` as-is, without evaluation.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxChecker check = check(expander, stx);
        check.arityExact(2);
        return stx;
    }


    //========================================================================


    @Override
    CompiledForm compile(Compiler comp, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        Evaluator eval = comp.getEvaluator();
        SyntaxValue quotedSource = stx.get(eval, 1);
        Object result = quotedSource.syntaxToDatum(eval);
        return new CompiledConstant(result);
    }
}
