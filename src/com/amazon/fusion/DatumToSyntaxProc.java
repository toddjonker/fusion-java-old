// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

class DatumToSyntaxProc
    extends Procedure
{
    DatumToSyntaxProc()
    {
        //    "                                                                               |
        super("Converts the DATUM to a syntax object with the binding information copied from\n" +
              "the CONTEXT syntax identifier.",
              "context", "datum");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        SyntaxSymbol context = checkSyntaxSymbolArg(0, args);
        SyntaxValue datum = checkSyntaxArg(1, args);
         // TODO allow other types and convert

        return Syntax.datumToSyntax(context, datum);
    }
}
