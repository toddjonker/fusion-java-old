// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;

final class ForEachFieldProc
    extends Procedure
{
    ForEachFieldProc()
    {
        //    "                                                                               |
        super("Applies PROC to each field within STRUCT, ignoring any results.\n" +
              "PROC must take two arguments, a field name and an Ion value.\n" +
              "Returns STRUCT.",
              "proc", "struct");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(args);

        Procedure proc = checkProcArg(0, args);
        IonStruct s = checkStructArg(1, args);

        for (IonValue field : s)
        {
            String name = field.getFieldName();
            IonString nameDom = eval.getSystem().newString(name);

            Object nameValue  = new DomValue(nameDom);
            Object fieldValue = new DomValue(field);

            eval.callNonTail(proc, nameValue, fieldValue);
        }

        return args[1]; // Don't need to re-wrap the input struct
    }
}
