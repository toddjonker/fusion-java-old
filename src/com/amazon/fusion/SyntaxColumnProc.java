// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionVoid.voidValue;


class SyntaxColumnProc
    extends Procedure1
{
    SyntaxColumnProc()
    {
        //    "                                                                               |
        super("Returns the column number of the source of a syntax object.",
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
            return makeInt(eval, location.myColumn);
        }
        return voidValue(eval);
    }
}
