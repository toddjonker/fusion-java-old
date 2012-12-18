// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWriteToString;


final class MacroTransformer
    extends MacroForm
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
            expanded = eval.callNonTail(myTransformer, source);
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
                safeWriteToString(eval, expanded);
            throw new SyntaxFailure(myTransformer.identify(), message,
                                    source);
        }
    }
}
