// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;
import java.util.List;

final class SyntaxSubseqProc
    extends Procedure
{
    SyntaxSubseqProc()
    {
        //    "                                                                               |
        super("Returns a new syntax sexp that copies children from SEQUENCE starting at FROM.",
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

        List<SyntaxValue> subseq = sequence.myChildren.subList(from, size);
        // That's a view into the original sequence, make a stand-alone copy
        // so changes to the original don't affect our result.
        ArrayList<SyntaxValue> children =
            new ArrayList<SyntaxValue>(subseq.size());
        children.addAll(subseq);

        SyntaxSexp result = SyntaxSexp.make(/* location */ null, children);
        return result;
    }
}
