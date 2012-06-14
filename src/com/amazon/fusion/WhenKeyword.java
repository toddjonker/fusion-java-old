// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;

/**
 * The {@code when} syntactic form.
 */
final class WhenKeyword
    extends MacroValue
{
    WhenKeyword()
    {
        super("COND BODY",
              "Checks if COND is satisfies and execute BODY if it is, otherwise UNDEF " +
              "returned.");
    }

    /**
     * Performs a single "level" of macro expansion.
     * Transforms
     * {@code (when c b...)}
     * to
     * {@code (if c (begin b...) undef}
     *
     * @param env
     * @param expr the input expression, including the keyword symbol.
     * @return
     */
    @Override
    IonValue expand(IonSexp expr)
        throws SyntaxFailure
    {
        int whenExprSize = expr.size();
        if (whenExprSize < 2)
        {
           throw new SyntaxFailure(getEffectiveName(), "", expr);
        }

        int bindingPos = 1;

        ValueFactory vf = expr.getSystem();
        IonSexp ifForm = vf.newEmptySexp();

        ifForm.add().newSymbol("if");
        IonValue conditionForm = expr.get(1).clone();
        ifForm.add(conditionForm);

        IonSexp beginForm = ifForm.add().newEmptySexp();
        beginForm.add().newSymbol("begin");
        for (int i = bindingPos + 1; i < whenExprSize; i++)
        {
            IonValue bodyForm = expr.get(i).clone();
            beginForm.add(bodyForm);
        }
        ifForm.add().newSymbol("undef");

        return ifForm;
    }
}
