// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.DomValue.assumeStruct;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;

class ForEachFieldFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function 'for_each_field'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(for_each_field FUNC STRUCT)\n\n");
        out.write("Applies FUNC to each field within STRUCT, ignoring any results.\n" +
                  "FUNC must take two arguments, a field name and an Ion value.\n" +
                  "Returns STRUCT.\n");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        FunctionValue func = (FunctionValue) args[0];

        IonStruct s = assumeStruct(args[1]);

        for (IonValue field : s)
        {
            String name = field.getFieldName();
            IonString nameDom = eval.getSystem().newString(name);
            DomValue nameValue = new DomValue(nameDom);

            DomValue fieldValue = new DomValue(field);

            FusionValue[] funcArgs = new FusionValue[]{ nameValue, fieldValue };
            func.invoke(eval, funcArgs);
        }

        return args[1];
    }
}
