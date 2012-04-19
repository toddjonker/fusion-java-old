// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.DomValue.assumeStruct;
import static com.amazon.fusion.DomValue.assumeTextContent;
import com.amazon.ion.IonStruct;

class RemoveFunction
    extends FunctionValue
{
    RemoveFunction()
    {
        //    "                                                                               |
        super("Removes all fields from STRUCT that have the given NAMEs. Returns STRUCT.",
              "struct", "name", DOTDOTDOT);
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonStruct s = assumeStruct(args[0]);

        for (int i = 1; i < args.length; i++)
        {
            String fieldName = assumeTextContent(args[i]);
            s.remove(fieldName);
        }

        return args[0];
    }
}
