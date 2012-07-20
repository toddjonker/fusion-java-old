// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
     * {@code (if c (begin b...) undef)}
     *
     * @param env
     * @param expr the input expression, including the keyword symbol.
     * @return
     */
    @Override
    SyntaxValue expand(SyntaxSexp expr)
        throws SyntaxFailure
    {
        int whenExprSize = expr.size();
        if (whenExprSize < 2)
        {
           throw new SyntaxFailure(getEffectiveName(), "", expr);
        }

        SyntaxValue conditionForm = expr.get(1);

        SyntaxSexp beginForm = BeginKeyword.makeSyntax(expr, 2);

        SyntaxSexp ifForm = SyntaxSexp.make(SyntaxSymbol.make("if"),
                                            conditionForm,
                                            beginForm,
                                            SyntaxSymbol.make("undef"));
        return ifForm;
    }
}
