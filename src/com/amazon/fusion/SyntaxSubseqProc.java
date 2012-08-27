// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class SyntaxSubseqProc
    extends Procedure
{
    SyntaxSubseqProc()
    {
        //    "                                                                               |
        super("Returns a new syntax sexp that copies children from SEQUENCE starting at FROM.\n" +
            "If FROM is beyond the end of the SEQUENCE, the result is an empty sequence.",
              "sequence", "from");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);
        SyntaxSequence sequence = checkSyntaxSequenceArg(0, args);
        int from = (int) checkLongArg(1, args); // TODO type-safety
        int size = sequence.size();

        if (size < from) from = size;

        SyntaxSequence result = sequence.makeSubseq(from, size);
        return result;
    }
}
