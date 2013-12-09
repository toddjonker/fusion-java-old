// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.FusionVoid.voidValue;


class SyntaxSourceProc
    extends Procedure1
{
    SyntaxSourceProc()
    {
        //    "                                                                               |
        super("Returns a description of the source of a syntax object.",
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
            SourceName name = location.myName;
            if (name != null)
            {
                return makeString(eval, name.display());
            }
        }
        return voidValue(eval);
    }
}
