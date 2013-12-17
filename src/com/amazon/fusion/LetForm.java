// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSymbol.isSymbol;

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
              "result is the result of the entire expression.\n" +
              "\n" +
              "    (let loop_id [(ident expr), ...] body ...+)\n" +
              "\n" +
              "This variant also creates a procedure, bound to the given name `loop_id`, that\n" +
              "accepts the same number of arguments as there are `ident`s. When invoked, the\n" +
              "procedure binds the `ident`s to the arguments and evaluates the body.\n" +
              "\n" +
              "For example, this snippet loops through the standard input stream and writes\n" +
              "the `title` field of each item:\n" +
              "\n" +
              "    (let loop [(item (read))]\n" +
              "      (unless (is_eof item)\n" +
              "        (let [(title (. item \"title\"))]\n" +
              "          (writeln title)\n" +
              "          (loop (read)))))");
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
        throws FusionException
    {
        final Evaluator eval = expander.getEvaluator();

        SyntaxChecker check = check(eval, stx);
        final int letExprSize = check.arityAtLeast(3);

        SyntaxValue loopName = null;
        int bindingPos = 1;
        {
            SyntaxValue maybeName =
                check.requiredForm("loop name or binding pairs", 1);
            if (isSymbol(eval, maybeName.unwrap(eval)))
            {
                loopName = maybeName;
                bindingPos = 2;
            }
        }
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

        SyntaxSexp formals = SyntaxSexp.make(eval, subforms);

        // Build the lambda
        subforms = new SyntaxValue[letExprSize - bindingPos + 1];
        subforms[0] = eval.getGlobalState().myKernelLambdaIdentifier;
        subforms[1] = formals;
        for (int i = bindingPos + 1; i < letExprSize; i++)
        {
            SyntaxValue bodyForm = stx.get(eval, i);
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
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(eval, i);
            subforms[i + 1] = binding.get(eval, 1);
        }

        return SyntaxSexp.make(eval, stx.getLocation(), subforms);
    }
}
