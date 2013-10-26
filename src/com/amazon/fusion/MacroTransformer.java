// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.safeWriteToString;


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
    SyntaxValue doExpandOnce(Expander expander, SyntaxSexp stx)
        throws SyntaxException
    {
        Object expanded;
        try
        {
            // TODO FUSION-32 This should set current-namespace
            // See Racket Reference 1.2.3.2
            // http://docs.racket-lang.org/reference/syntax-model.html#(part._expand-steps)
            expanded = expander.getEvaluator().callNonTail(myTransformer, stx);
        }
        catch (SyntaxException e)
        {
            e.addContext(stx);
            throw e;
        }
        catch (FusionException e)
        {
            String message =
                "Error expanding macro: " + e.getMessage();
            SyntaxException fail =
                new SyntaxException(getInferredName(), message, stx);
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
            throw new SyntaxException(myTransformer.identify(), message,
                                      stx);
        }
    }
}
