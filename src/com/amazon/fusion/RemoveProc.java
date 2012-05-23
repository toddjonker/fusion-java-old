// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonStruct;

final class RemoveProc
    extends Procedure
{
    RemoveProc()
    {
        //    "                                                                               |
        super("Removes all fields from STRUCT that have the given NAMEs, returning STRUCT.\n" +
              "If STRUCT is null.struct, this procedure has no effect.",
              "struct", "name", DOTDOTDOT);
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        IonStruct s = checkStructArg(0, args);

        for (int i = 1; i < args.length; i++)
        {
            String fieldName = checkTextArg(i, args);
            s.remove(fieldName);
        }

        return args[0];
    }
}
