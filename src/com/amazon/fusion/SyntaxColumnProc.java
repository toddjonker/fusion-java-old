// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;


class SyntaxColumnProc
    extends Procedure1
{
    SyntaxColumnProc()
    {
        //    "                                                                               |
        super("Returns the one-based column number of the source of a syntax object.\n" +
              "The result is zero if there's no column number available.",
              "stx");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        SyntaxValue stx = checkSyntaxArg(0, arg);
        SourceLocation location = stx.getLocation();
        if (location != null)
        {
            return makeInt(eval, location.getColumn());
        }
        return FusionNumber.ZERO_INT;
    }
}
