// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.isList;
import static com.amazon.fusion.FusionList.unsafeListAdd;
import static com.amazon.fusion.FusionSexp.unsafeSexpAdd;


final class AddProc
    extends Procedure2
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
    Object doApply(Evaluator eval, Object sequence, Object element)
        throws FusionException
    {
        checkSequenceArg(eval, 0, sequence, element);

        if (isList(eval, sequence))
        {
            return unsafeListAdd(eval, sequence, element);
        }

        return unsafeSexpAdd(eval, sequence, element);
    }
}
