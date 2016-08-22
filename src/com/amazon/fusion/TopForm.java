// Copyright (c) 2013-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Implementation of {@code #%top}, which is introduced when identifiers are
 * macro-expanded in a scope where the identifier is not bound.
 */
final class TopForm
    extends SyntacticForm
{
    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        SyntaxChecker check = check(eval, stx);
        check.arityExact(2);

        SyntaxSymbol id =
            check.requiredIdentifier("namespace-level variable", 1);

        SyntaxValue[] children = stx.extract(eval);

        Namespace ns = env.namespace();
        if (ns instanceof TopLevelNamespace)
        {
            // https://docs.racket-lang.org/reference/__top.html says:
            //
            //   In a top-level context, (#%top . id) always refers to a
            //   top-level variable, even if id is unbound or otherwise bound.

            Binding binding = ns.resolveDefinition(id);
            if (binding == null)
            {
                // There's no top-level definition with the same marks, so just
                // lookup by name.

                SyntaxSymbol stripped = id.stripWraps(eval);
                assert stripped.resolve() instanceof FreeBinding;

                binding = ns.resolveDefinition(stripped);
                // This may still be free, but don't fail until eval-time.
                // We'd like things like (expand (#%top foo)) to succeed.

                if (binding == null)
                {
                    binding = stripped.getBinding();
                }
            }

            id = id.copyReplacingBinding(binding);
        }
        else
        {
            assert ns instanceof ModuleNamespace;

            // (#%top id) inside module expands to id, but it must be defined
            // at the module level.

            SyntaxSymbol topId = (SyntaxSymbol) children[0];

            Binding binding = id.resolve();
            if (ns.ownsDefinedBinding(binding))
            {
                id = (SyntaxSymbol) id.trackOrigin(eval, stx, topId);
                return id;
            }
            else if (binding instanceof FreeBinding)
            {
                // The identifier may be from a different lexical context.
                // Let's look it up as-is.
                binding = ns.resolveDefinition(id);
                if (binding != null)
                {
                    id = (SyntaxSymbol) id.trackOrigin(eval, stx, topId);
                    return id.copyReplacingBinding(binding);
                }
            }

            throw new UnboundIdentifierException(id);
        }

        children[1] = id;
        return stx.copyReplacingChildren(eval, children);
    }

    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxSymbol id = (SyntaxSymbol) stx.get(eval, 1);

        return id.resolve().compileTopReference(eval, env, id);
    }
}
