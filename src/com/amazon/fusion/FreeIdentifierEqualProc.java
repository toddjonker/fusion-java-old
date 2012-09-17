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
              "identifier1", "identifier2");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        SyntaxSymbol id1 = checkSyntaxSymbolArg(0, args);
        SyntaxSymbol id2 = checkSyntaxSymbolArg(1, args);

        return id1.freeIdentifierEqual(eval, id2);
    }
}
