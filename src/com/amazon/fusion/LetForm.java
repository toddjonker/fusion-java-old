// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class LetForm
    extends MacroForm
{
    LetForm()
    {
        //    "                                                                               |
        super("((ident expr) ...) body ...+",
              "Evaluates the `expr`s left to right, then binds each `ident` to its\n" +
              "corresponding result, then evaluates `body`.  The scope of the `ident`s only\n" +
              "covers the `body`, not the `expr`s.\n" +
              "\n" +
              "`body` may be one or more forms; the last form is in tail position and its\n" +
              "result is the result of the entire expression.");
    }

    /**
     * Expands
     * {@code (let ((v e) ...) b ...)}
     * to
     * {@code ((lambda (v ...) b ...) e ...)}
     * <p>
     * Expands
     * {@code (let f ((v e) ...) b ...)}
     * to
     * {@code ((letrec ((f (lambda (v ...) b ...))) f) e ...)}
     */
    @Override
    SyntaxValue doExpandOnce(Expander expander, SyntaxSexp stx)
        throws SyntaxFailure
    {
        SyntaxChecker check = check(stx);
        final int letExprSize = check.arityAtLeast(3);

        SyntaxSymbol loopName = determineLoopName(check);
        int bindingPos = (loopName == null ? 1 : 2);
        if (letExprSize < bindingPos + 2)
        {
            throw check.failure("no body");
        }

        SyntaxChecker checkBindings =
            check.subformSeq("sequence of bindings", bindingPos);
        SyntaxSequence bindingForms = checkBindings.form();

        // Build the lambda's formal parameter list
        int bindingCount = bindingForms.size();
        SyntaxValue[] subforms = new SyntaxValue[bindingCount];
        for (int i = 0; i < bindingCount; i++)
        {
            SyntaxChecker checkPair =
                checkBindings.subformSexp("binding pair", i);
            checkPair.arityExact(2);
            SyntaxSymbol boundName =
                checkPair.requiredIdentifier("bound name", 0);

            subforms[i] = boundName;
        }

        Evaluator eval = expander.getEvaluator();
        SyntaxSexp formals = SyntaxSexp.make(eval, subforms);

        // Build the lambda
        subforms = new SyntaxValue[letExprSize - bindingPos + 1];
        subforms[0] = expander.getGlobalState().myKernelLambdaIdentifier;
        subforms[1] = formals;
        for (int i = bindingPos + 1; i < letExprSize; i++)
        {
            SyntaxValue bodyForm = stx.get(i);
            subforms[i - bindingPos + 1] = bodyForm;
        }
        SyntaxSexp lambdaForm = SyntaxSexp.make(eval, subforms);


        // Build the outer result expression
        subforms = new SyntaxValue[bindingForms.size() + 1];

        if (loopName != null)
        {
            SyntaxSexp binding  = SyntaxSexp.make(eval, loopName, lambdaForm);
            SyntaxSexp bindings = SyntaxSexp.make(eval, binding);
            SyntaxSexp letrec   =
                SyntaxSexp.make(eval,
                                eval.getGlobalState().myKernelLetrecIdentifier,
                                bindings,
                                loopName);
            subforms[0] = letrec;
        }
        else
        {
            subforms[0] = lambdaForm;
        }

        for (int i = 0; i < bindingForms.size(); i++)
        {
            // Already type- and arity-checked this above
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            subforms[i + 1] = binding.get(1);
        }

        return SyntaxSexp.make(expander, stx.getLocation(), subforms);
    }

    SyntaxSymbol determineLoopName(SyntaxChecker check)
        throws SyntaxFailure
    {
        SyntaxValue maybeName =
            check.requiredForm("loop name or binding pairs", 1);
        if (maybeName.getType() == SyntaxValue.Type.SYMBOL)
        {
            return (SyntaxSymbol) maybeName;
        }
        return null;
    }
}
