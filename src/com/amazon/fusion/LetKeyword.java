// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
    SyntaxValue expand(Evaluator eval, SyntaxSexp source)
        throws SyntaxFailure
    {
        SyntaxSymbol loopName = checkForName(source);
        int bindingPos = (loopName == null ? 1 : 2);

        final int letExprSize = source.size();
        if (letExprSize < bindingPos + 2)
        {
            throw new SyntaxFailure(getEffectiveName(), "", source);
        }

        SyntaxSequence bindingForms =
            requiredSequence("sequence of bindings", bindingPos, source);
        int bindingCount = bindingForms.size();

        // Build the lambda's formal parameter list
        SyntaxValue[] subforms = new SyntaxValue[bindingCount];
        for (int i = 0; i < bindingCount; i++)
        {
            SyntaxValue bindingForm = bindingForms.get(i);
            SyntaxSexp binding =
                requiredSexp("name/value binding", bindingForm);
            SyntaxSymbol boundName =
                requiredSymbol("name/value binding", 0, binding);

            subforms[i] = boundName;
        }
        SyntaxSexp formals = SyntaxSexp.make(null, subforms);


        // Build the lambda
        subforms = new SyntaxValue[letExprSize - bindingPos + 1];
        subforms[0] = eval.makeKernelIdentifier("lambda");
        subforms[1] = formals;
        for (int i = bindingPos + 1; i < letExprSize; i++)
        {
            SyntaxValue bodyForm = source.get(i);
            subforms[i - bindingPos + 1] = bodyForm;
        }
        SyntaxSexp lambdaForm = SyntaxSexp.make(null, subforms);


        // Build the outer result expression
        subforms = new SyntaxValue[bindingForms.size() + 1];

        if (loopName != null)
        {
            SyntaxSexp binding  = SyntaxSexp.make(loopName, lambdaForm);
            SyntaxSexp bindings = SyntaxSexp.make(binding);
            SyntaxSexp letrec   =
                SyntaxSexp.make(eval.makeKernelIdentifier("letrec"),
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
            // Already type-checked this above
            SyntaxSexp binding = (SyntaxSexp) bindingForms.get(i);
            SyntaxValue boundValue =
                requiredForm("name/value binding", 1, binding);

            subforms[i + 1] = boundValue;
        }

        SyntaxSexp result = SyntaxSexp.make(source.getLocation(), subforms);
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
