// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code quote} syntactic form.
 */
final class QuoteKeyword
    extends KeywordValue
{
    QuoteKeyword()
    {
        //    "                                                                               |
        super("DATUM",
              "Returns the Ion DATUM as-is, without evaluation.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
    {
        SyntaxValue quotedStx = expr.get(1);
        return quotedStx.quote(eval);
    }
}
