// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 * The {@code quote} syntactic form.
 */
final class QuoteKeyword
    extends KeywordValue
{
    QuoteKeyword()
    {
        super("quote", "DATUM",
              "Returns the Ion DATUM as-is, without evaluation.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        IonValue datum = expr.get(1);
        return new DomValue(datum);
    }
}
