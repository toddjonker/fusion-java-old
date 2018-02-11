// Copyright (c) 2012-2018 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

class FreeIdentifierEqualProc
    extends Procedure
{
    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(2, args);
        SyntaxSymbol id1 = checkSyntaxSymbolArg(0, args);
        SyntaxSymbol id2 = checkSyntaxSymbolArg(1, args);

        return id1.freeIdentifierEqual(eval, id2);
    }
}
