// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;

/**
 * The {@code func} syntactic form, which evaluates to a {@link FuncValue}.
 */
final class FuncKeyword
    extends KeywordValue
{
    FuncKeyword(String keyword)
    {
        super(keyword, "PARAM DOC? EXPR",
              "Returns a single-parameter function.  When invoked, the caller's argument is\n" +
              "bound to PARAM and the body EXPR is evaluated and returned.\n" +
              "DOC is an optional documentation string.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        return new FuncValue(expr, env);
    }
}
