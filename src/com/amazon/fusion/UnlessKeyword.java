// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.BaseModule.BEGIN;
import static com.amazon.fusion.BaseModule.IF;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import com.amazon.ion.ValueFactory;

/**
 * The {@code when} syntactic form.
 */
final class UnlessKeyword
    extends MacroValue
{
    UnlessKeyword()
    {
        super("COND BODY",
              "Checks if COND is satisfies and execute BODY if it is not, otherwise UNDEF " +
              "returned.");
    }

    /**
     * Performs a single "level" of macro expansion.
     * Transforms
     * {@code (unless c b...)}
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
        int unlessExprSize = expr.size();
        if (unlessExprSize < 2)
        {
           throw new SyntaxFailure(getEffectiveName(), "", expr);
        }

        int bindingPos = 1;

        ValueFactory vf = expr.getSystem();
        IonSexp ifForm = vf.newEmptySexp();

        ifForm.add().newSymbol(IF);
        IonValue conditionForm = expr.get(1).clone();
        ifForm.add(conditionForm);

        ifForm.add().newSymbol("undef");

        IonSexp beginForm = ifForm.add().newEmptySexp();
        beginForm.add().newSymbol(BEGIN);
        for (int i = bindingPos + 1; i < unlessExprSize; i++)
        {
            IonValue bodyForm = expr.get(i).clone();
            beginForm.add(bodyForm);
        }

        return ifForm;
    }

}
