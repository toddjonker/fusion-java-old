// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonStruct;

class RemoveFunction
    extends FunctionValue
{
    RemoveFunction()
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
        expectArityAtLeast(1, args);
        IonStruct s = assumeStructArg(0, args);

        for (int i = 1; i < args.length; i++)
        {
            String fieldName = assumeTextArg(i, args);
            s.remove(fieldName);
        }

        return args[0];
    }
}
