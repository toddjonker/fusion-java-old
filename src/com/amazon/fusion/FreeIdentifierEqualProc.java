// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

class FreeIdentifierEqualProc
    extends Procedure
{
    FreeIdentifierEqualProc()
    {
        //    "                                                                               |
        super("Compares two syntax symbols to determine whether they both refer to the same\n" +
              "binding.",
              "IDENT1", "IDENT2");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);
        SyntaxSymbol id1 = checkSyntaxSymbolArg(0, args);
        SyntaxSymbol id2 = checkSyntaxSymbolArg(1, args);

        return id1.freeIdentifierEqual(eval, id2);
    }
}
