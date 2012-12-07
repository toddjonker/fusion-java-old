// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.unsafeStructMerge;

final class StructMergeProc
    extends Procedure2
{
    StructMergeProc()
    {
        //    "                                                                               |
        super("Returns a struct that has all the name-value elements of both arguments.  This\n" +
              "will result in repeated fields if the names overlap or if one of the arguments\n" +
              "has repeats.",
              "struct1", "struct2");
    }

    @Override
    Object doApply(Evaluator eval, Object struct1, Object struct2)
        throws FusionException
    {
        checkStructArg(eval, 0, struct1, struct2);
        checkStructArg(eval, 1, struct1, struct2);

        return unsafeStructMerge(eval, struct1, struct2);
    }
}
