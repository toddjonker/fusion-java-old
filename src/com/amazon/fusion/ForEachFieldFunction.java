// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;

class ForEachFieldFunction
    extends FunctionValue
{
    ForEachFieldFunction()
    {
        //    "                                                                               |
        super("Applies FUNC to each field within STRUCT, ignoring any results.\n" +
              "FUNC must take two arguments, a field name and an Ion value.\n" +
              "Returns STRUCT.",
              "func", "struct");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        FunctionValue func = checkFuncArg(0, args);
        IonStruct s = checkStructArg(1, args);

        for (IonValue field : s)
        {
            String name = field.getFieldName();
            IonString nameDom = eval.getSystem().newString(name);
            DomValue nameValue = new DomValue(nameDom);

            DomValue fieldValue = new DomValue(field);

            eval.applyNonTail(func, nameValue, fieldValue);
        }

        return args[1]; // Don't need to re-wrap the input struct
    }
}
