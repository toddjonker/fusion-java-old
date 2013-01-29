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
    SyntaxValue expand(Evaluator eval, ExpandContext ctx, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(source);
        check.arityExact(2);

        SyntaxValue[] children = source.extract();
        SyntaxValue quoted = children[1];
        children[1] = quoted.stripWraps();

        source = SyntaxSexp.make(source.getLocation(), children);
        return source;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxValue quotedSource = source.get(1);
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
