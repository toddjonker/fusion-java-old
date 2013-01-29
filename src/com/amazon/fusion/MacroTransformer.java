// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
    SyntaxValue expandOnce(Expander expander, SyntaxSexp stx)
        throws SyntaxFailure
    {
        Object expanded;
        try
        {
            expanded = expander.getEvaluator().callNonTail(myTransformer, stx);
        }
        catch (SyntaxFailure e)
        {
            e.addContext(stx);
            throw e;
        }
        catch (FusionException e)
        {
            String message =
                "Error expanding macro: " + e.getMessage();
            SyntaxFailure fail =
                new SyntaxFailure(getInferredName(), message, stx);
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
                safeWriteToString(expander.getEvaluator(), expanded);
            throw new SyntaxFailure(myTransformer.identify(), message,
                                    stx);
        }
    }
}
