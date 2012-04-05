// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.DomValue.assumeStruct;
import static com.amazon.fusion.DomValue.assumeTextContent;
import com.amazon.ion.IonStruct;
import java.io.IOException;
import java.io.Writer;

class RemoveFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function 'remove'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(remove STRUCT NAME ...)\n\n");
        out.write("Removes all fields from STRUCT that have the given NAMEs.\n" +
                  "Returns STRUCT.\n");
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
