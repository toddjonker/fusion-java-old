// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkIntArgToJavaInt;

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
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        SyntaxSequence sequence = checkSyntaxSequenceArg(0, args);
        int from = checkIntArgToJavaInt(eval, this, 1, args);
        int size = sequence.size();

        if (size < from) from = size;

        sequence = sequence.makeSubseq(eval, from);
        if (sequence == null)
        {
            throw new ArgTypeFailure(this, "proper sequence", 0, args);
        }
        return sequence;
    }
}
