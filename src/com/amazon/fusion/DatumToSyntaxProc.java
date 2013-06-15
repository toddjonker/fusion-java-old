// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.Syntax.datumToSyntax;
import static com.amazon.fusion.Syntax.isIdentifier;


class DatumToSyntaxProc
    extends Procedure
{
    DatumToSyntaxProc()
    {
        //    "                                                                               |
        super("Converts the `datum` to a syntax object with the binding information copied\n" +
              "from the `context` syntax identifier.\n" +
              "\n" +
              "When `context` isn't provided, the resulting syntax object has no lexical\n" +
              "context.",
              "datum", "[context]");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityRange(1, 2, args);

        SyntaxSymbol context = null;
        Object datum = args[0];

        if (args.length == 2)
        {
            if (! isIdentifier(eval, args[1]))
            {
                throw argFailure("syntax identifier", 1, args);
            }
            context = (SyntaxSymbol) args[1];
        }

        return datumToSyntax(eval, datum, context);
    }
}
