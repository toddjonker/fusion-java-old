// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import com.amazon.fusion.BindingDoc.Kind;
import com.amazon.fusion.Namespace.TopBinding;

final class DefineSyntaxForm
    extends SyntacticForm
{
    DefineSyntaxForm()
    {
        //    "                                                                               |
        super("id xform",
              "Binds the identifier ID to a syntax transformer XFORM. The transformer must be\n" +
              "a procedure that accepts an syntax object and returns a syntax object.");
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxChecker check = check(source);
        int arity = check.arityAtLeast(3);

        SyntaxValue[] children = source.extract();

        SyntaxSymbol identifier = check.requiredIdentifier(1);

        // We need to strip off the module-level wrap that's already been
        // applied to the identifier. Otherwise we'll loop forever trying to
        // resolve it! This is a bit of a hack, really.
        SyntaxSymbol stripped = identifier.stripImmediateEnvWrap(env);

        // If at module top-level, this has already been done.
        // TODO FUSION-51 should know the context where this is happening...
        Namespace ns = env.namespace();
        ns.predefine(stripped);

        // Update the identifier with its binding.
        // This is just a way to pass the binding instance through to the
        // runtime stage so invoke() below can reuse it.
        children[1] = identifier.expand(eval, env);

        int bodyPos;
        SyntaxValue maybeDoc = children[2];
        if (maybeDoc.getType() == SyntaxValue.Type.STRING && arity > 3)
        {
            bodyPos = 3;
        }
        else
        {
            bodyPos = 2;
        }

        if (bodyPos != arity-1)
        {
            throw check.failure("Too many subforms");
        }

        SyntaxValue valueStx = source.get(bodyPos);
        children[bodyPos] = valueStx.expand(eval, env);

        source = SyntaxSexp.make(source.getLocation(), children);
        return source;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        int arity = source.size();
        SyntaxValue valueSource = source.get(arity-1);
        CompiledForm valueForm = eval.compile(env, valueSource);

        SyntaxSymbol identifier = (SyntaxSymbol) source.get(1);
        TopBinding binding = (TopBinding) identifier.getBinding();
        CompiledForm compiled =
            binding.compileDefineSyntax(eval, env, valueForm);

        if (arity != 3
            && eval.firstContinuationMark(COLLECT_DOCS_MARK) != null)
        {
            // We have documentation. Sort of.
            SyntaxString docString = (SyntaxString) source.get(2);
            BindingDoc doc = new BindingDoc(identifier.stringValue(),
                                            Kind.SYNTAX,
                                            null, // usage
                                            docString.stringValue());
            env.namespace().setDoc(binding.myAddress, doc);
        }

        return compiled;
    }
}
