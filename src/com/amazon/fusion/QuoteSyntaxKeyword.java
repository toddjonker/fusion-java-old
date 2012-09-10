// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class QuoteSyntaxKeyword
    extends KeywordValue
{
    QuoteSyntaxKeyword()
    {
        super("DATUM",
              "Returns a syntax object retaining the lexical information of DATUM.");
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        check(source).arityExact(2);

        return source;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxValue quotedStx = source.get(1);
        return quotedStx;
    }
}
