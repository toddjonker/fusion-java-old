// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BindingDoc.COLLECT_DOCS_MARK;
import static com.amazon.fusion.GlobalState.DEFINE;
import com.amazon.fusion.Namespace.NsBinding;

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


    /**
     * Predefines the identifiers at module level. As with all binding forms,
     * we modify the original identifier so that its
     * {@link SyntaxSymbol#getBinding()} returns the new binding.
     * This is a hack to communicate that information between the "expand" and
     * "compile" phases, since the complire doesn't have the same environment
     * and cannot resolve bindings.
     *
     * @return the {@code define} form, with the bound identifier updated to
     * have the new {@link Binding}.
     */
    static SyntaxSexp predefine(Evaluator eval,
                                ModuleNamespace ns,
                                SyntaxSexp defineStx,
                                SyntaxValue formForErrors)
        throws FusionException
    {
        SyntaxChecker check = new SyntaxChecker(eval, DEFINE, defineStx);
        check.arityAtLeast(3);

        SyntaxValue[] children = defineStx.extract(eval);

        SyntaxValue first = children[1];
        if (first.getType() == SyntaxValue.Type.SEXP)
        {
            SyntaxChecker sigCheck =
                check.subformSeq("procedure signature", 1);

            SyntaxSymbol identifier =
                sigCheck.requiredIdentifier("procedure name", 0);

            SyntaxValue[] sig = ((SyntaxSexp) first).extract(eval);
            sig[0] = ns.predefine(identifier, formForErrors);
            children[1] = SyntaxSexp.make(eval,
                                          first.getLocation(),
                                          first.getAnnotations(),
                                          sig);
        }
        else
        {
            SyntaxSymbol identifier = check.requiredIdentifier(1);
            children[1] = ns.predefine(identifier, formForErrors);
        }

        return SyntaxSexp.make(eval, defineStx.getLocation(), defineStx.getAnnotations(),
                               children);
    }

    // (define (p f ...) d? b ...+) => (define p d? (lambda (f ...) b ...))
    SyntaxSexp expandImplicitLambda(Expander expander, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxChecker defineChecker = check(expander, stx);
        int defineArity = defineChecker.arityAtLeast(3);

        Evaluator eval = expander.getEvaluator();
        if (stx.get(eval, 1).getType() != SyntaxValue.Type.SEXP)
        {
            // No implicit lambda
            return stx;
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

        SyntaxValue[] origDefineElts = stx.extract(eval);
        boolean hasDoc =
            (defineArity > 3 &&
             origDefineElts[2].getType() == SyntaxValue.Type.STRING);
        int docOffset = (hasDoc ? 1 : 0);

        int bodyStart = 2 + docOffset;
        int bodyLen = defineArity - bodyStart;
        assert bodyLen > 0;

        SyntaxValue[] lambda = new SyntaxValue[2 + bodyLen];
        lambda[0] = expander.getGlobalState().myKernelLambdaIdentifier;
        lambda[1] = SyntaxSexp.make(expander, procFormals);
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
        newDefineElts[2 + docOffset] =
            SyntaxSexp.make(expander, lambda);

        return SyntaxSexp.make(expander, stx.getLocation(), newDefineElts);
    }


    @Override
    SyntaxValue expand(Expander expander, Environment env, SyntaxSexp origStx)
        throws FusionException
    {
        // Two phase expansion.
        // TODO rewrite this as a macro, replace this entire built-in form
        // with define_values.
        SyntaxSexp stx = expandImplicitLambda(expander, origStx);

        SyntaxChecker check = check(expander, stx);
        if (! (expander.isTopLevelContext() || expander.isModuleContext()))
        {
            throw check.failure("Definition must be at top-level or module level");
        }

        int arity = check.arityAtLeast(3);

        Evaluator eval = expander.getEvaluator();
        SyntaxValue[] children = stx.extract(eval);

        // If we had an implicit lambda, this will always succeed since we've
        // already checked the identifier.  Thus this will only cause an error
        // when using the original stx, so we know the error trace will
        // correctly refer to the original syntax.
        SyntaxSymbol identifier = check.requiredIdentifier(1);

        // If at module context, this has already been done.
        if (expander.isTopLevelContext())
        {
            Namespace ns = env.namespace();
            assert ns == env;
            identifier = ns.predefine(identifier, origStx);
            children[1] = identifier;
        }

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

        SyntaxValue valueStx = stx.get(eval, bodyPos);
        children[bodyPos] = expander.expandExpression(env, valueStx);

        stx = SyntaxSexp.make(expander, stx.getLocation(), children);
        return stx;
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
        NsBinding binding = (NsBinding) identifier.getBinding();
        CompiledForm compiled = binding.compileDefine(eval, env, valueForm);

        if (arity != 3
            && eval.firstContinuationMark(COLLECT_DOCS_MARK) != null)
        {
            // We have documentation. Sort of.
            SyntaxString docString = (SyntaxString) stx.get(eval, 2);
            BindingDoc doc = new BindingDoc(identifier.stringValue(),
                                            null, // kind
                                            null, // usage
                                            docString.stringValue());
            env.namespace().setDoc(binding.myAddress, doc);
        }

        return compiled;
    }
}
