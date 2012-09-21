// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


public class SyntaxLineProc
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
            return eval.newInt(location.myLine);
        }
        return UNDEF;
    }
}
