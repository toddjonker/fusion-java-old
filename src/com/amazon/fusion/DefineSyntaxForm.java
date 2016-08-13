// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.stringToJavaString;
import com.amazon.fusion.BindingDoc.Kind;
import com.amazon.fusion.Namespace.NsDefinedBinding;

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
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        SyntaxChecker check = check(eval, stx);
        if (! (expander.isTopLevelContext() || expander.isModuleContext()))
        {
            throw check.failure("Definition must be at top-level or module level");
        }

        int arity = check.arityAtLeast(3);

        SyntaxValue[] children = stx.extract(eval);

        SyntaxSymbol identifier = check.requiredIdentifier(1);

        // WARNING!  This isn't conditional as with 'define' since
        // 'define_syntax' doesn't get predefined by the 'module' expander.
        {
            Namespace ns = env.namespace();
            assert ns == env;
            children[1] = ns.predefine(identifier, stx);
        }

        int bodyPos;
        SyntaxValue maybeDoc = children[2];
        if (isString(eval, maybeDoc.unwrap(eval)) && arity > 3)
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

        SyntaxValue valueStx = stx.get(eval, bodyPos);
        children[bodyPos] = expander.expandExpression(env, valueStx);

        return stx.copyReplacingChildren(eval, children);
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        int arity = stx.size();
        SyntaxValue valueSource = stx.get(eval, arity-1);
        CompiledForm valueForm = eval.compile(env, valueSource);

        SyntaxSymbol identifier = (SyntaxSymbol) stx.get(eval, 1);
        NsDefinedBinding binding = (NsDefinedBinding) identifier.getBinding();
        CompiledForm compiled =
            binding.compileDefineSyntax(eval, env, valueForm);

        if (arity != 3
            && eval.firstContinuationMark(COLLECT_DOCS_MARK) != null)
        {
            // We have documentation. Sort of.
            Object docString = stx.get(eval, 2).unwrap(eval);
            BindingDoc doc = new BindingDoc(identifier.stringValue(),
                                            Kind.SYNTAX,
                                            null, // usage
                                            stringToJavaString(eval, docString));
            env.namespace().setDoc(binding.myAddress, doc);
        }

        return compiled;
    }
}
