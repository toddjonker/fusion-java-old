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
        super("((IDENT EXPR) ...) BODY",
              "Binds each IDENT to its EXPR, then evaluates BODY.\n" +
              "BODY may be one or more forms; the result of the last form is the result of the\n" +
              "entire let-expression.");
    }

    /**
     * Expands
     * {@code (let ((v e) ...) b)} to {@code ((func (v ...) b) e ...)}
     */
    @Override
    IonValue expand(IonSexp expr)
    {
        IonSequence bindingForms = (IonSequence) expr.get(1);

        ValueFactory vf = expr.getSystem();

        IonSexp result = vf.newEmptySexp();
        IonSexp function = result.add().newEmptySexp();
        function.add().newSymbol("func");
        IonSexp formals = function.add().newEmptySexp();
        for (int i = 2; i < expr.size(); i++)
        {
            IonValue bodyForm = expr.get(i).clone();
            function.add(bodyForm);
        }

        for (IonValue bindingForm : bindingForms)
        {
            IonSexp bindingPair = (IonSexp) bindingForm;
            IonSymbol formal = (IonSymbol) bindingPair.get(0);
            IonValue arg = bindingPair.get(1);

            formals.add(formal.clone());
            result.add(arg.clone());
        }

        return result;
    }
}
