// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class QuoteSyntaxForm
    extends SyntacticForm
{
    QuoteSyntaxForm()
    {
        super("DATUM",
              "Returns a syntax object retaining the lexical information of DATUM.");
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        check(expander, stx).arityExact(2);

        return stx;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxValue quoted = stx.get(eval, 1);
        return new CompiledQuoteSyntax(quoted);
    }


    //========================================================================


    static final class CompiledQuoteSyntax
        implements CompiledForm
    {
        private final SyntaxValue myQuoted;

        CompiledQuoteSyntax(SyntaxValue quoted)
        {
            myQuoted = quoted;
        }

        @Override
        public SyntaxValue doEval(Evaluator eval, Store store)
            throws FusionException
        {
            return myQuoted;
        }
    }
}
