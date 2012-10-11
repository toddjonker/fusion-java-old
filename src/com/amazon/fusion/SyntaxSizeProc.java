// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class SyntaxSizeProc
    extends Procedure1
{
    SyntaxSizeProc()
    {
        //    "                                                                               |
        super("Returns the number of child elements contained in the syntax sequence.\n" +
              "The size of null.list and null.sexp is zero.",
              "sequence");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        SyntaxSequence c = checkSyntaxSequenceArg(0, arg);
        return eval.newInt(c.size());
    }
}
