// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
        super("DATUM",
              "Returns the Ion DATUM as-is, without evaluation.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(stx);
        check.arityExact(2);

        SyntaxValue[] children = stx.extract();
        SyntaxValue quoted = children[1];
        children[1] = quoted.stripWraps();

        stx = SyntaxSexp.make(stx.getLocation(), children);
        return stx;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxValue quotedSource = stx.get(1);
        return new CompiledQuote(quotedSource);
    }


    //========================================================================


    private static final class CompiledQuote
        implements CompiledForm
    {
        // TODO FUSION-35 don't retain syntax tree, it has too much info
        private final SyntaxValue mySyntax;

        CompiledQuote(SyntaxValue syntax)
        {
            mySyntax = syntax;
        }


        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return mySyntax.quote(eval);
        }
    }
}
