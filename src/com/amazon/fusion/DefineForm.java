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
        super("id value",
              "Binds a top-level variable `id` to the result of `value`.\n" +
              "\n" +
              "    (define (id arg ...) body ...+)\n" +
              "\n" +
              "Defines a procedure `id`, with formal arguments `arg ...` and the `body`.\n" +
              "This form is equivalent to `(define id (lambda (arg ...) body ...))`.");
    }


    static SyntaxSymbol boundIdentifier(Evaluator eval, Environment env,
                                        SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker check = new SyntaxChecker("define", source);
        check.arityAtLeast(3);

        if (source.get(1).getType() == SyntaxValue.Type.SEXP)
        {
            SyntaxChecker sigCheck =
                check.subformSeq("procedure signature", 1);

            return sigCheck.requiredIdentifier("procedure name", 0);
        }

        SyntaxSymbol identifier = check.requiredIdentifier(1);
        return identifier;
    }

    // (define (p f ...) d? b ...+) => (define p d? (lambda (f ...) b ...))
    SyntaxSexp expandImplicitLambda(Evaluator eval, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxChecker defineChecker = check(source);
        int defineArity = defineChecker.arityAtLeast(3);

        if (source.get(1).getType() != SyntaxValue.Type.SEXP)
        {
            // No implicit lambda
            return source;
        }

        SyntaxChecker check =
            defineChecker.subformSeq("procedure signature", 1);
        int sigArity = check.arityAtLeast(1);

        SyntaxSymbol procName = check.requiredIdentifier("procedure name", 0);

        SyntaxValue[] procFormals = new SyntaxValue[sigArity - 1];
        for (int i = 1; i < sigArity; i++)
        {
            procFormals[i-1] =
                check.requiredIdentifier("procedure formal argument", i);
        }

        SyntaxValue[] origDefineElts = source.extract();
        boolean hasDoc =
            (defineArity > 3 &&
             origDefineElts[2].getType() == SyntaxValue.Type.STRING);
        int docOffset = (hasDoc ? 1 : 0);

        int bodyStart = 2 + docOffset;
        int bodyLen = defineArity - bodyStart;
        assert bodyLen > 0;

        SyntaxValue[] lambda = new SyntaxValue[2 + bodyLen];
        lambda[0] = eval.makeKernelIdentifier("lambda");
        lambda[1] = SyntaxSexp.make(procFormals);
        for (int p = 2, i = bodyStart; i < defineArity; p++, i++)
        {
            lambda[p] = origDefineElts[i];
        }

        SyntaxValue[] newDefineElts = new SyntaxValue[3 + docOffset];
        newDefineElts[0] = origDefineElts[0];   // define
        newDefineElts[1] = procName;
        if (hasDoc)
        {
            newDefineElts[2] = origDefineElts[2];
        }
        newDefineElts[2 + docOffset] = SyntaxSexp.make(lambda);

        return SyntaxSexp.make(source.getLocation(), newDefineElts);
    }


    @Override
    SyntaxValue expand(Evaluator eval, Environment env, SyntaxSexp source)
        throws FusionException
    {
        // Two phase expansion.
        // TODO rewrite this as a macro, replace this entire built-in form
        // with define_values.
        source = expandImplicitLambda(eval, source);

        SyntaxChecker check = check(source);
        int arity = check.arityAtLeast(3);

        SyntaxValue[] children = source.extract();

        // If check2 != check, this will always succeed since we've already
        // checked the identifier.  So this will only cause an error when
        // check2 == check so we know the error trace will correctly refer to
        // the original syntax.
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
            // Use the original checker so error trace has original syntax.
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
