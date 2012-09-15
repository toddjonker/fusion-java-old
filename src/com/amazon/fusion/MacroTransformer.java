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
    SyntaxValue expandOnce(Evaluator eval, SyntaxSexp source)
        throws SyntaxFailure
    {
        Object expanded;
        try
        {
            expanded = eval.applyNonTail(myTransformer, source);
        }
        catch (SyntaxFailure e)
        {
            e.addContext(source);
            throw e;
        }
        catch (FusionException e)
        {
            String message =
                "Error expanding macro: " + e.getMessage();
            SyntaxFailure fail =
                new SyntaxFailure(getInferredName(), message, source);
            fail.initCause(e);
            throw fail;
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
                                    source);
        }
    }
}
