// Copyright (c) 2012-2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.isSexp;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.FusionString.unsafeStringToJavaString;
import static com.amazon.fusion.GlobalState.DEFINE;
import static com.amazon.fusion.GlobalState.LAMBDA;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import com.amazon.fusion.FusionString.BaseString;
import com.amazon.fusion.FusionSymbol.BaseSymbol;

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
     * "compile" phases, since the compiler doesn't have the same environment
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
        if (isSexp(eval, first.unwrap(eval)))
        {
            SyntaxChecker sigCheck =
                check.subformSeq("procedure signature", 1);

            SyntaxSymbol identifier =
                sigCheck.requiredIdentifier("procedure name", 0);

            SyntaxSexp signature = (SyntaxSexp) first;
            SyntaxValue[] sigChildren = signature.extract(eval);
            sigChildren[0] = ns.predefine(identifier, formForErrors);

            children[1] = signature.copyReplacingChildren(eval, sigChildren);
        }
        else
        {
            SyntaxSymbol identifier = check.requiredIdentifier(1);
            children[1] = ns.predefine(identifier, formForErrors);
        }

        return defineStx.copyReplacingChildren(eval, children);
    }

    // (define (p f ...) d? b ...+) => (define p d? (lambda (f ...) b ...))
    SyntaxSexp expandImplicitLambda(Expander expander, SyntaxSexp stx)
        throws FusionException
    {
        SyntaxChecker defineChecker = check(expander, stx);
        int defineArity = defineChecker.arityAtLeast(3);

        Evaluator eval = expander.getEvaluator();
        if (! isSexp(eval, stx.get(eval, 1).unwrap(eval)))
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
        SyntaxValue docStx = origDefineElts[2];
        boolean hasDoc =
            (defineArity > 3 &&
             isString(eval, docStx.unwrap(eval)));
        int docOffset = (hasDoc ? 1 : 0);

        int bodyStart = 2 + docOffset;
        int bodyLen = defineArity - bodyStart;
        assert bodyLen > 0;

        SyntaxValue[] lambda = new SyntaxValue[2 + bodyLen];
        lambda[0] = eval.getGlobalState().kernelBoundIdentifier(eval, LAMBDA);
        lambda[1] = SyntaxSexp.make(eval, procFormals);
        for (int p = 2, i = bodyStart; i < defineArity; p++, i++)
        {
            lambda[p] = origDefineElts[i];
        }

        SyntaxValue[] newDefineElts = new SyntaxValue[3 + docOffset];
        newDefineElts[0] = origDefineElts[0];   // define
        newDefineElts[1] = procName;
        if (hasDoc)
        {
            // Prefix the documentation string with the procedure signature.
            BaseString docStr = (BaseString) docStx.unwrap(eval);
            String doc = unsafeStringToJavaString(eval, docStr);
            if (! (doc.startsWith("    ") || doc.startsWith("\n    ")))
            {
                String newDoc = "\n    " + origDefineElts[1] + "\n" + doc;
                BaseSymbol[] annotations = docStr.getAnnotations();
                docStr = makeString(eval, newDoc).annotate(eval, annotations);
                docStx = makeSyntax(eval, docStx.getLocation(), docStr);
            }

            newDefineElts[2] = docStx;
        }
        newDefineElts[2 + docOffset] = SyntaxSexp.make(eval, lambda);

        return stx.copyReplacingChildren(eval, newDefineElts);
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
        check.requiredIdentifier(1);

        // https://docs.racket-lang.org/reference/define.html says:
        //   At the top level, the top-level binding for id is created after
        //   evaluating expr, if it does not exist already, and the top-level
        //   mapping of id (in the namespace linked with the compiled
        //   definition) is set to the binding at the same time.
        // This works differently from our predefine(), used at module level.

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
            // Use the original checker so error trace has original syntax.
            throw check.failure("Too many subforms");
        }

        SyntaxValue valueStx = stx.get(eval, bodyPos);
        children[bodyPos] = expander.expandExpression(env, valueStx);

        return stx.copyReplacingChildren(eval, children);
    }


    //========================================================================


    @Override
    void evalCompileTimePart(Compiler comp,
                             TopLevelNamespace topNs,
                             SyntaxSexp topStx)
        throws FusionException
    {
        Evaluator eval = comp.getEvaluator();
        SyntaxSymbol identifier = (SyntaxSymbol) topStx.get(eval, 1);
        topNs.predefine(identifier, identifier);
    }


    //========================================================================


    @Override
    CompiledForm compile(Compiler comp, Environment env, SyntaxSexp stx)
        throws FusionException
    {
        return comp.compileDefine(env, stx);
    }
}
