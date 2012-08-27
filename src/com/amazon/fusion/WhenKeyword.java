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
     * @param expr the input expression, including the keyword symbol.
     */
    @Override
    SyntaxValue expand(Evaluator eval, SyntaxSexp expr)
        throws SyntaxFailure
    {
        int whenExprSize = expr.size();
        if (whenExprSize < 2)
        {
           throw new SyntaxFailure(getEffectiveName(), "", expr);
        }

        SyntaxValue conditionForm = expr.get(1);

        SyntaxSexp beginForm = BeginKeyword.makeSyntax(eval, expr, 2);

        SyntaxSexp ifForm = SyntaxSexp.make(expr.getLocation(),
                                            eval.makeKernelIdentifier("if"),
                                            conditionForm,
                                            beginForm,
                                            eval.makeKernelIdentifier("undef"));
        return ifForm;
    }
}
