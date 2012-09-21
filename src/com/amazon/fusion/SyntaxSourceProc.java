// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


public class SyntaxSourceProc
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
                return eval.newString(name.display());
            }
        }
        return UNDEF;
    }
}
