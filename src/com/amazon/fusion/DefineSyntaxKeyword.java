// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Namespace.NsBinding;

final class DefineSyntaxKeyword
    extends KeywordValue
{
    DefineSyntaxKeyword()
    {
        //    "                                                                               |
        super("ID XFORM",
              "Defines a syntax keyword ID with the given syntax transformer XFORM. The\n" +
              "transformer must be a procedure that accepts an Ion value and returns and Ion\n" +
              "value.");
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(source);
        check.arityExact(3);

        SyntaxSymbol identifier = check.requiredIdentifier(1);

        // We need to strip off the module-level wrap that's already been
        // applied to the identifier. Otherwise we'll loop forever trying to
        // resolve it! This is a bit of a hack, really.
        SyntaxSymbol stripped = identifier.stripImmediateEnvWrap(env);

        // If at module top-level, this has already been done.
        // TODO we should know the context where this is happening...
        Namespace ns = env.namespace();
        ns.predefine(stripped);

        // Update the identifier with its binding.
        // This is just a way to pass the binding instance through to the
        // runtime stage so invoke() below can reuse it.
        identifier = (SyntaxSymbol) identifier.expand(eval, env);

        SyntaxValue valueStx = source.get(2);
        valueStx = valueStx.expand(eval, env);

        source = SyntaxSexp.make(source.getLocation(),
                                 source.get(0), identifier, valueStx);
        return source;
    }


    //========================================================================


    @Override
    CompiledForm compile(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        SyntaxValue valueSource = source.get(2);
        CompiledForm valueForm = eval.compile(env, valueSource);

        SyntaxSymbol identifier = (SyntaxSymbol) source.get(1);
        NsBinding binding = (NsBinding) identifier.getBinding();
        assert binding != null;

        return new CompiledDefineSyntax(binding, valueForm);
    }


    //========================================================================


    private final class CompiledDefineSyntax
        implements CompiledForm
    {
        private final NsBinding    myBinding;
        private final CompiledForm myValueForm;

        private CompiledDefineSyntax(NsBinding binding, CompiledForm valueForm)
        {
            myBinding   = binding;
            myValueForm = valueForm;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            // TODO this shouldn't do anything during phase-0 evaluation.
            Object value = eval.eval(store, myValueForm);

            if (value instanceof Procedure)
            {
                Procedure xformProc = (Procedure) value;
                value = new MacroTransformer(xformProc);
            }
            else if (! (value instanceof KeywordValue))
            {
                String message =
                    "value is not a transformer: " + writeToString(value);
                throw contractFailure(message);
            }

            RuntimeNamespace ns = store.runtimeNamespace();
            ns.bindPredefined(myBinding, value);
            return UNDEF;
        }
    }
}
