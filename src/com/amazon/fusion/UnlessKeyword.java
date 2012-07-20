// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * The {@code unless} syntactic form.
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
     * {@code (if c undef (begin b...))}
     *
     * @param env
     * @param expr the input expression, including the keyword symbol.
     * @return
     */
    @Override
    SyntaxValue expand(SyntaxSexp expr)
        throws SyntaxFailure
    {
        int unlessExprSize = expr.size();
        if (unlessExprSize < 2)
        {
           throw new SyntaxFailure(getEffectiveName(), "", expr);
        }

        SyntaxValue conditionForm = expr.get(1);

        SyntaxSexp beginForm = BeginKeyword.makeSyntax(expr, 2);

        SyntaxSexp ifForm = SyntaxSexp.make(SyntaxSymbol.make("if"),
                                            conditionForm,
                                            SyntaxSymbol.make("undef"),
                                            beginForm);
        return ifForm;
    }
}
