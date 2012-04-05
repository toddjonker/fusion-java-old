// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonSexp;
import java.io.IOException;
import java.io.Writer;

/**
 * The {@code help} built-in function.
 * <p>
 * TODO add a Documentation model and move this to user space.
 */
final class HelpFunction
    extends FunctionValue
{
    @Override
    final void print(Writer out)
        throws IOException
    {
        out.write("// Function 'help'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(help OBJ ...)\n\n");
        out.write("Prints the documentation of the given objects, if available.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
    {
        FusionValue result = new FusionValue()
        {
            @Override
            void print(Writer out)
                throws IOException
            {
                for (FusionValue arg : args)
                {
                    out.write('\n');
                    arg.printHelp(out);
                }
            }

            @Override
            FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
            {
                throw new IonException("not invokable");
            }
        };

        return result;
    }
}
