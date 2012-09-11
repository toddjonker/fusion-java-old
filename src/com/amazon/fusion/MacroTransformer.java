// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class MacroTransformer
    extends MacroValue
{
    private final Procedure myTransformer;


    MacroTransformer(Procedure transformer)
    {
        super(null, null); // TODO Get docs from declaration, like procedures?
        myTransformer = transformer;
    }


    @Override
    SyntaxValue expandOnce(Evaluator eval, SyntaxSexp expr)
        throws SyntaxFailure
    {
        Object expanded;
        try
        {
            expanded = eval.applyNonTail(myTransformer, expr);
        }
        catch (SyntaxFailure e)
        {
            e.addContext(expr);
            throw e;
        }
        catch (FusionException e)
        {
            String message =
                "Error expanding macro: " + e.getMessage();
            throw new SyntaxFailure(getInferredName(), message, expr);
        }

        try
        {
            return (SyntaxValue) expanded;
        }
        catch (ClassCastException e)
        {
            String message =
                "Transformer returned non-syntax result: " +
                 writeToString(expanded);
            throw new SyntaxFailure(myTransformer.identify(), message,
                                    expr);
        }
    }
}
