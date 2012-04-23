// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonSexp;
import com.amazon.ion.IonSymbol;
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
     * {@code (let ((v e) ...) b ...)} to {@code ((func (v ...) b ...) e ...)}
     */
    @Override
    IonValue expand(IonSexp expr)
        throws SyntaxFailure
    {
        final int letExprSize = expr.size();
        if (letExprSize < 3)
        {
            throw new SyntaxFailure(getEffectiveName(), "", expr);
        }
        IonSequence bindingForms =
            requiredSequence("sequence of bindings", 1, expr);

        ValueFactory vf = expr.getSystem();

        IonSexp result = vf.newEmptySexp();
        IonSexp function = result.add().newEmptySexp();
        function.add().newSymbol("func");
        IonSexp formals = function.add().newEmptySexp();
        for (int i = 2; i < letExprSize; i++)
        {
            IonValue bodyForm = expr.get(i).clone();
            function.add(bodyForm);
        }

        for (IonValue bindingForm : bindingForms)
        {
            IonSexp binding =
                requiredSexp("name/value binding", bindingForm);
            IonSymbol name = requiredSymbol("name/value binding", 0, binding);
            IonValue arg = requiredForm("name/value binding", 1, binding);

            formals.add(name.clone());
            result.add(arg.clone());
        }

        return result;
    }
}
