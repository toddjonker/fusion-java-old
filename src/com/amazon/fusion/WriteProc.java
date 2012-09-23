// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.IOException;
import java.io.OutputStream;

final class WriteProc
    extends Procedure
{
    WriteProc()
    {
        //    "                                                                               |
        super("Writes a value as Ion data to the standard output stream.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        IonTextWriterBuilder b = IonTextWriterBuilder.pretty();
        IonWriter writer = b.build((OutputStream) System.out);

        FusionValue.write(writer, args[0]);

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
