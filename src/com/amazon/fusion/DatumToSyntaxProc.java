// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.Syntax.datumToSyntax;
import static com.amazon.fusion.Syntax.isIdentifier;


class DatumToSyntaxProc
    extends Procedure2
{
    DatumToSyntaxProc()
    {
        //    "                                                                               |
        super("Converts the DATUM to a syntax object with the binding information copied from\n" +
              "the CONTEXT syntax identifier.",
              "context", "datum");
    }

    @Override
    Object doApply(Evaluator eval, Object arg0, Object arg1)
        throws FusionException
    {
        SyntaxSymbol context;
        if (isIdentifier(eval, arg0))
        {
            context = (SyntaxSymbol) arg0;
        }
        else if (isNullNull(eval, arg0))
        {
            context = null;
        }
        else
        {
            throw argFailure("syntax identifier, or null", 0, arg0, arg1);
        }

        return datumToSyntax(eval, context, arg1);
    }
}
