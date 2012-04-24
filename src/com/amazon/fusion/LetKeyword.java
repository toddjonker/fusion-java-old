// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;

/**
 *
 */
class LetKeyword
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
     * {@code ((func (v ...) b ...) e ...)}
     * <p>
     * Expands
     * {@code (let f ((v e) ...) b ...)}
     * to
     * {@code ((letrec ((f (func (v ...) b ...))) f) e ...)}
     */
    @Override
    IonValue expand(IonSexp letExpr)
        throws SyntaxFailure
    {
        String loopName = checkForName(letExpr);
        int bindingPos = (loopName == null ? 1 : 2);

        final int letExprSize = letExpr.size();
        if (letExprSize < bindingPos + 2)
        {
            throw new SyntaxFailure(getEffectiveName(), "", letExpr);
        }

        IonSequence bindingForms =
            requiredSequence("sequence of bindings", bindingPos, letExpr);

        ValueFactory vf = letExpr.getSystem();
        IonSexp result = vf.newEmptySexp();

        IonSexp function;
        if (loopName != null)
        {
            IonSexp letrec = result.add().newEmptySexp();
            letrec.add().newSymbol("letrec");
            IonSexp bindings = letrec.add().newEmptySexp();
            IonSexp binding = bindings.add().newEmptySexp();
            binding.add().newSymbol(loopName);
            function = binding.add().newEmptySexp();
            letrec.add().newSymbol(loopName);
        }
        else
        {
            function = result.add().newEmptySexp();
        }

        function.add().newSymbol("func");
        IonSexp formals = function.add().newEmptySexp();
        for (int i = bindingPos + 1; i < letExprSize; i++)
        {
            IonValue bodyForm = letExpr.get(i).clone();
            function.add(bodyForm);
        }

        for (IonValue bindingForm : bindingForms)
        {
            IonSexp binding =
                requiredSexp("name/value binding", bindingForm);
            IonSymbol boundName =
                requiredSymbol("name/value binding", 0, binding);
            IonValue boundValue =
                requiredForm("name/value binding", 1, binding);

            formals.add(boundName.clone());
            result.add(boundValue.clone());
        }

        return result;
    }

    String checkForName(IonSexp letExpr)
        throws SyntaxFailure
    {
        IonValue maybeName = requiredForm("", 1, letExpr);
        if (maybeName.getType() == IonType.SYMBOL)
        {
            return ((IonSymbol) maybeName).stringValue();
        }
        return null;
    }
}
