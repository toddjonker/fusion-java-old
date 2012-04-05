// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;

/**
 * The {@code func} syntactic form, which evaluates to a {@link FuncValue}.
 */
final class FuncKeyword
    extends KeywordValue
{
    FuncKeyword()
    {
        super("func", "(PARAM ...) DOC? BODY",
              "Returns a new function.  When invoked, the caller's arguments are bound to the\n" +
              "PARAMs and the BODY is evaluated and returned.\n" +
              "DOC is an optional documentation string.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "function invocation.");
    }

    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
    {
        return new FuncValue(expr, env);
    }
}
