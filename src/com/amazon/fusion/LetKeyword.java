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
        super("let", "((IDENT EXPR) ...) BODY",
              "Binds each IDENT to its EXPR, then evaluates BODY.");
    }

    /**
     * Expands
     * {@code (let ((v e) ...) b)} to {@code ((func (v ...) b) e ...)}
     */
    @Override
    IonValue expand(IonSexp expr)
    {
        IonSequence bindingForms = (IonSequence) expr.get(1);
        IonValue body = expr.get(2);

        ValueFactory vf = expr.getSystem();

        IonSexp result = vf.newEmptySexp();
        IonSexp function = result.add().newEmptySexp();
        function.add().newSymbol("func");
        IonSexp formals = function.add().newEmptySexp();
        function.add(body.clone());

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
