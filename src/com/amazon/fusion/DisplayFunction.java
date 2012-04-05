// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;

/**
 *
 */
class DisplayFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function 'display'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(display OBJ ...)\n\n");
        out.write("Outputs objects to stdout in human-readable form.\n" +
                  "The result is not necessarily Ion data.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        try
        {
            OutputStreamWriter out = new OutputStreamWriter(System.out);
            try
            {
                for (FusionValue arg : args)
                {
                    arg.display(out);
                }
            }
            finally
            {
                out.flush();
            }
        }
        catch (IOException e)
        {
            throw new IonException(e);
        }

        return UNDEF;
    }
}
