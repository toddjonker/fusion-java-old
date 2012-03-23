// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonSexp;
import java.io.IOException;
import java.io.Writer;

class DocFunction
    extends FunctionValue
{
    @Override
    final void print(Writer out)
        throws IOException
    {
        out.write("// Function 'doc'\n");
    }

    @Override
    void printDoc(Writer out)
        throws IOException
    {
        out.write("(doc VALUE)\n\n");
        out.write("Prints the documentation of a given value, if available.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue arg)
    {
        FusionValue result = new FusionValue()
        {
            @Override
            void print(Writer out)
                throws IOException
            {
                out.write('\n');
                arg.printDoc(out);
            }

            @Override
            FusionValue invoke(Evaluator eval, Environment context, IonSexp expr)
            {
                throw new IonException("not invokable");
            }
        };

        return result;
    }
}
