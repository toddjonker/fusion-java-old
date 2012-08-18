// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Base class for Fusion macros, performing syntax expansion.
 */
abstract class MacroValue
    extends KeywordValue
{
    MacroValue(String bodyPattern, String doc)
    {
        super(bodyPattern, doc);
    }


    @Override
    SyntaxValue prepare(Evaluator eval, Environment env, SyntaxSexp stx)
        throws SyntaxFailure
    {
        SyntaxValue expanded = expand(stx);
        return expanded.prepare(eval, env);
    }

    /**
     * Performs a single "level" of macro expansion.
     *
     * @param env
     * @param expr the input expression, including the keyword symbol.
     * @return
     */
    abstract SyntaxValue expand(SyntaxSexp expr)
        throws SyntaxFailure;


    @Override
    final FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        throw new IllegalStateException("Macros should be expanded already!");
    }
}
