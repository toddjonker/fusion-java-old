// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSexp.unsafeSexpAdd;
import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorAdd;


final class AddProc
    extends Procedure
{
    AddProc()
    {
        //    "                                                                               |
        super("Returns a sequence with all the elements of `sequence` and the `element`.  The\n" +
              "result sequence is similar to the input, its size is one greater, and it may\n" +
              "share structure with other sequences.\n" +
              "\n" +
              "In general, the position of `element` within the result is not specified, but\n" +
              "particular sequence types may do so.\n" +
              "\n" +
              "* For lists, the `element` becomes the last element of the result.\n" +
              "* For sexps, the `element` becomes the head of the result.",
              "sequence", "element");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        // TODO FUSION-87 this makes extra copies when converting
        Object s = coerceSequenceArg(eval, 0, args);

        if (isVector(eval, s))
        {
            return unsafeVectorAdd(eval, s, args[1]);
        }

        return unsafeSexpAdd(eval, s, args[1]);
    }
}
