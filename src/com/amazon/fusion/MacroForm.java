// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * Base class for Fusion macros, performing syntax expansion.
 */
abstract class MacroForm
    extends SyntacticForm
{
    private static final AtomicInteger ourMarkCounter = new AtomicInteger();

    MacroForm(String bodyPattern, String doc)
    {
        super(bodyPattern, doc);
    }


    @Override
    final SyntaxValue expand(Evaluator eval, Expander ctx,
                             Environment env, SyntaxSexp source)
        throws FusionException
    {
        // TODO FUSION-39 we create two MarkWrap instances here
        final int mark = ourMarkCounter.incrementAndGet();

        source = (SyntaxSexp) source.addOrRemoveMark(mark);

        SyntaxValue expanded = expandOnce(eval, source);
        expanded = expanded.addOrRemoveMark(mark);

        // TODO tail
        return eval.expand(ctx, env, expanded);
    }

    /**
     * Performs a single "level" of macro expansion.
     *
     * @param source the input expression, including the macro identifier.
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
