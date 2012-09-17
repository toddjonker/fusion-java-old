// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class SyntaxAppendProc
    extends Procedure
{
    SyntaxAppendProc()
    {
        //    "                                                                               |
        super("Returns a new syntax sexp that combines the HEAD and TAIL sequences.",
              "head", "tail");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        SyntaxSequence head = checkSyntaxSequenceArg(0, args);
        SyntaxSequence tail = checkSyntaxSequenceArg(1, args);
        SyntaxSequence result = head.makeAppended(tail);
        return result;
    }
}
