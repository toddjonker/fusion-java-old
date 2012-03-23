// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;


abstract class FusionValue
{
    IonValue getDom()
    {
        return null;
    }


    abstract void print(Writer out)
        throws IOException;


    /**
     * Prints the documentation of this value.
     * Implementations should try to ensure that a final newline is printed.
     *
     * @param out the output stream, not null.
     */
    void printDoc(Writer out)
        throws IOException
    {
        out.write("// No documentation.\n");
    }


    abstract FusionValue invoke(Evaluator eval,
                                Environment context,
                                IonSexp expr);
}
