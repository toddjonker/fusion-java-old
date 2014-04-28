// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class SyntaxAppendProc
    extends Procedure
{
    SyntaxAppendProc()
    {
        //    "                                                                               |
        super("Returns a new syntax sexp that combines the HEAD and TAIL sequences.",
              "head", "tail", DOTDOTDOT);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        SyntaxSequence seq = checkSyntaxSequenceArg(0, args);
        for (int i = 1; i < args.length; i++)
        {
            SyntaxSequence next = checkSyntaxSequenceArg(i, args);
            seq = seq.makeAppended(eval, next);
            if (seq == null)
            {
                throw new ArgumentException(this, "proper sequence", i-1, args);
            }
        }
        return seq;
    }
}
