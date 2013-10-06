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


    final SyntaxValue expandOnce(Expander expander, SyntaxSexp stx)
        throws FusionException
    {
        // TODO FUSION-39 we create two MarkWrap instances here
        final int mark = ourMarkCounter.incrementAndGet();

        stx = (SyntaxSexp) stx.addOrRemoveMark(mark);

        SyntaxValue expanded = doExpandOnce(expander, stx);

        return expanded.addOrRemoveMark(mark);
    }

    @Override
    final SyntaxValue expand(Expander expander, Environment env,
                             SyntaxSexp stx)
        throws FusionException
    {
        SyntaxValue expanded = expandOnce(expander, stx);

        // TODO FUSION-207 tail expand
        return expander.expand(env, expanded);
    }

    /**
     * PRIVATE! Performs a single "level" of macro expansion.
     * Not for use outside this hierarchy.
     *
     * @param stx the input syntax, including the macro identifier.
     * @throws FusionException
     */
    abstract SyntaxValue doExpandOnce(Expander expander, SyntaxSexp stx)
        throws FusionException;


    @Override
    final CompiledForm compile(Evaluator eval, Environment env,
                               SyntaxSexp stx)
        throws FusionException
    {
        throw new IllegalStateException("Macros should be expanded already!");
    }
}
