// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;

/**
 *
 */
class WriteFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function 'write'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(write VAL)\n\n");
        out.write("Writes a value as Ion data to the standard output stream.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        Writeable value = (Writeable) args[0];

        IonTextWriterBuilder b = IonTextWriterBuilder.pretty();
        IonWriter writer = b.build((OutputStream) System.out);
        value.write(writer);
        try
        {
            writer.flush();
        }
        catch (IOException e)
        {
            throw new RuntimeException(e);
        }
        System.out.println();
        return null;
    }
}
