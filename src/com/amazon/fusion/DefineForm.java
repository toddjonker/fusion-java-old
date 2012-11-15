// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import com.amazon.fusion.Namespace.TopBinding;

final class DefineForm
    extends SyntacticForm
{
    DefineForm()
    {
        //    "                                                                               |
        super("var value",
              "Defines a top-level variable VAR with the given VALUE.");
    }


    static SyntaxSymbol boundIdentifier(Evaluator eval, Environment env,
                                        SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = new SyntaxChecker("define", source);
        check.arityAtLeast(3);

        SyntaxSymbol identifier = check.requiredIdentifier(1);
        return identifier;
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
        //identifier = identifier.stripImmediateEnvWrap(env);
        SyntaxSymbol stripped = identifier.stripImmediateEnvWrap(env);

        // If at module top-level, this has already been done.
        // TODO FUSION-51 should know the context where this is happening...
        Namespace ns = env.namespace();
        ns.predefine(stripped);

        // Update the identifier with its binding.
        // This is just a way to pass the binding instance through to the
        // runtime stage so compile() below can reuse it.
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
        CompiledForm compiled = binding.compileDefine(eval, env, valueForm);

        if (arity != 3
            && eval.firstContinuationMark(COLLECT_DOCS_MARK) != null)
        {
            // We have documentation. Sort of.
            SyntaxString docString = (SyntaxString) source.get(2);
            BindingDoc doc = new BindingDoc(identifier.stringValue(),
                                            null, // kind
                                            null, // usage
                                            docString.stringValue());
            env.namespace().setDoc(binding.myAddress, doc);
        }

        return compiled;
    }
}
