// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Implementation of {@code #%top}.
 */
final class TopForm
    extends SyntacticForm
{
    TopForm()
    {
        super("(#%top ID)", "XXX");
    }

    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        // TODO if inside module, expand to the id

        SyntaxChecker check = check(stx);
        check.arityExact(2);

        SyntaxSymbol id = check.requiredIdentifier("top-level variable", 1);
        Binding binding = id.resolve();

        return stx;
    }

    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxSymbol id = (SyntaxSymbol) stx.get(1);

        return id.resolve().compileTopReference(eval, env, id);
    }
}
