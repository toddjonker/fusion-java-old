// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.unsafeVectorAddM;

final class AddMProc
    extends Procedure2
{
    AddMProc()
    {
        //    "                                                                               |
        super("Returns a sequence with all the elements of `list` and the `element`.  The\n" +
              "result sequence is similar to the input, its size is one greater, and it may\n" +
              "share structure with other sequences.  The input sequence may be mutated.\n" +
              "\n" +
              "In general, the position of `element` within the result is not specified, but\n" +
              "particular sequence types may do so.\n" +
              "\n" +
              "* For stretchy lists, the `element` is added to the end of the input sequence,\n" +
              "  which is then returned.",
              "list", "element");
    }


    @Override
    Object doApply(Evaluator eval, Object list, Object element)
        throws FusionException
    {
        checkListArg(eval, 0, list, element);
        return unsafeVectorAddM(eval, list, element);
    }
}
