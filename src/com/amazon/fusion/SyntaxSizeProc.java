// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class SyntaxSizeProc
    extends Procedure
{
    SyntaxSizeProc()
    {
        //    "                                                                               |
        super("Returns the number of child elements contained in the syntax sequence.\n" +
              "The size of null.list and null.sexp is zero.",
              "sequence");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        SyntaxSequence c = checkSyntaxSequenceArg(0, args);
        return eval.newInt(c.size());
    }
}
