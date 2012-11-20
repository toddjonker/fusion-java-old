// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.unsafeVectorAddM;

final class AddMProc
    extends Procedure
{
    AddMProc()
    {
        //    "                                                                               |
        super("Returns a sequence with all the elements of `sequence` and the `element`.  The\n" +
              "result sequence is similar to the input, its size is one greater, and it may\n" +
              "share structure with other sequences.  The input sequence may be mutated.\n" +
              "\n" +
              "In general, the position of `element` within the result is not specified, but\n" +
              "particular sequence types may do so.\n" +
              "\n" +
              "* For stretchy lists, the `element` is added to the end of the input sequence,\n" +
              "  which is then returned.",
              "sequence", "element");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        // TODO FUSION-87 this makes extra copies when converting IonList
        Object vector = coerceListArg(eval, 0, args);
        return unsafeVectorAddM(eval, vector, args[1]);
    }
}
