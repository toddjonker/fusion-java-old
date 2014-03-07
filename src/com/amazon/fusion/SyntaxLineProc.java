// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionVoid.voidValue;


class SyntaxLineProc
    extends Procedure1
{
    SyntaxLineProc()
    {
        //    "                                                                               |
        super("Returns the line number of the source of a syntax object.",
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
            return makeInt(eval, location.getLine());
        }
        return voidValue(eval);
    }
}
