// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 *
 */
final class LetKeyword
    extends MacroValue
{
    LetKeyword()
    {
        //    "                                                                               |
        super("((IDENT EXPR) ...) BODY ...+",
              "Binds each IDENT to its EXPR, then evaluates BODY.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "entire expression.");
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
    SyntaxValue expand(Evaluator eval, SyntaxSexp letExpr)
        throws SyntaxFailure
    {
        SyntaxSymbol loopName = checkForName(letExpr);
        int bindingPos = (loopName == null ? 1 : 2);

        final int letExprSize = letExpr.size();
        if (letExprSize < bindingPos + 2)
        {
            throw new SyntaxFailure(getEffectiveName(), "", letExpr);
        }

        SyntaxSequence bindingForms =
            requiredSequence("sequence of bindings", bindingPos, letExpr);

        SyntaxSexp result = SyntaxSexp.makeEmpty();

        SyntaxSexp lambdaForm = SyntaxSexp.makeEmpty();
        if (loopName != null)
        {
            SyntaxSexp binding  = SyntaxSexp.make(loopName, lambdaForm);
            SyntaxSexp bindings = SyntaxSexp.make(binding);
            SyntaxSexp letrec   =
                SyntaxSexp.make(eval.makeKernelIdentifier("letrec"),
                                bindings,
                                loopName);
            result.add(letrec);
        }
        else
        {
            result.add(lambdaForm);
        }

        lambdaForm.add(eval.makeKernelIdentifier("lambda"));
        SyntaxSexp formals = SyntaxSexp.makeEmpty();
        lambdaForm.add(formals);
        for (int i = bindingPos + 1; i < letExprSize; i++)
        {
            SyntaxValue bodyForm = letExpr.get(i);
            lambdaForm.add(bodyForm);
        }

        for (int i = 0; i < bindingForms.size(); i++)
        {
            SyntaxValue bindingForm = bindingForms.get(i);
            SyntaxSexp binding =
                requiredSexp("name/value binding", bindingForm);
            SyntaxSymbol boundName =
                requiredSymbol("name/value binding", 0, binding);
            SyntaxValue boundValue =
                requiredForm("name/value binding", 1, binding);

            formals.add(boundName);
            result.add(boundValue);
        }

        return result;
    }

    SyntaxSymbol checkForName(SyntaxSexp letExpr)
        throws SyntaxFailure
    {
        SyntaxValue maybeName = requiredForm("", 1, letExpr);
        if (maybeName.getType() == SyntaxValue.Type.SYMBOL)
        {
            return (SyntaxSymbol) maybeName;
        }
        return null;
    }
}
