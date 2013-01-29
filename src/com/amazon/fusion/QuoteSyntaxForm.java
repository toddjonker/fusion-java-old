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
    SyntaxValue expand(Evaluator eval, Expander ctx, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        check(source).arityExact(2);

        return source;
    }


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxValue quoted = source.get(1);
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
