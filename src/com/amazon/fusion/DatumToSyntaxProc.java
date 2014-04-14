// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSyntax.unsafeSyntaxLocation;
import static com.amazon.fusion.Syntax.datumToSyntax;
import static com.amazon.fusion.Syntax.isIdentifier;


class DatumToSyntaxProc
    extends Procedure
{
    DatumToSyntaxProc()
    {
        //    "                                                                               |
        super("Recursively converts the `datum` to a syntax object with lexical information\n" +
              "copied from the `context` syntax identifier, and source-location copied from\n" +
              "the `location` syntax object. Existing syntax objects in `datum` are included\n" +
              "as-is.\n" +
              "\n" +
              "When `context` isn't provided, converted syntax objects have no lexical\n" +
              "context.  When `location` isn't provided, they will have no location.",
              "datum", "[context location]");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityRange(1, 3, args);

        Object         datum    = args[0];
        SyntaxSymbol   context  = null;
        SourceLocation location = null;

        if (args.length > 1)
        {
            // TODO FUSION-329 This should accept arbitrary syntax objects.
            if (! isIdentifier(eval, args[1]))
            {
                throw argFailure("syntax identifier", 1, args);
            }
            context = (SyntaxSymbol) args[1];

            if (args.length > 2)
            {
                if (! Syntax.isSyntax(eval, args[2]))
                {
                    throw argFailure("syntax object", 2, args);
                }
                location = unsafeSyntaxLocation(eval, args[2]);
            }
        }

        return datumToSyntax(eval, datum, context, location);
    }
}
