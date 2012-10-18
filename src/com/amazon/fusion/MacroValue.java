// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * Base class for Fusion macros, performing syntax expansion.
 */
abstract class MacroValue
    extends KeywordValue
{
    private static final AtomicInteger ourMarkCounter = new AtomicInteger();

    MacroValue(String bodyPattern, String doc)
    {
        super(bodyPattern, doc);
    }


    @Override
    final SyntaxValue expand(Evaluator eval, Environment env,
                             SyntaxSexp source)
        throws FusionException
    {
        // TODO FUSION-39 we create two MarkWrap instances here
        final int mark = ourMarkCounter.incrementAndGet();

        source = (SyntaxSexp) source.addOrRemoveMark(mark);

        SyntaxValue expanded = expandOnce(eval, source);
        expanded = expanded.addOrRemoveMark(mark);

        return expanded.expand(eval, env);
    }

    /**
     * Performs a single "level" of macro expansion.
     *
     * @param source the input expression, including the keyword symbol.
     */
    abstract SyntaxValue expandOnce(Evaluator eval, SyntaxSexp source)
        throws SyntaxFailure;


    @Override
    final CompiledForm compile(Evaluator eval, Environment env,
                               SyntaxSexp source)
        throws FusionException
    {
        throw new IllegalStateException("Macros should be expanded already!");
    }
}
