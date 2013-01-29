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
    final SyntaxValue expand(Expander expander, Environment env,
                             SyntaxSexp stx)
        throws FusionException
    {
        // TODO FUSION-39 we create two MarkWrap instances here
        final int mark = ourMarkCounter.incrementAndGet();

        stx = (SyntaxSexp) stx.addOrRemoveMark(mark);

        SyntaxValue expanded = expandOnce(expander, stx);
        expanded = expanded.addOrRemoveMark(mark);

        // TODO tail
        return expander.expand(env, expanded);
    }

    /**
     * Performs a single "level" of macro expansion.
     *
     * @param stx the input syntax, including the macro identifier.
     */
    abstract SyntaxValue expandOnce(Expander expander, SyntaxSexp stx)
        throws SyntaxFailure;


    @Override
    final CompiledForm compile(Evaluator eval, Environment env,
                               SyntaxSexp stx)
        throws FusionException
    {
        throw new IllegalStateException("Macros should be expanded already!");
    }
}
