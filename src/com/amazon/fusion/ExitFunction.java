// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;
import java.io.Writer;

/**
 *
 */
class ExitFunction
    extends FunctionValue
{
    @Override
    void print(Writer out) throws IOException
    {
        out.write("// Function 'exit'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(exit)\n\n");
        out.write("Exits the running program.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] arg)
    {
        throw new Language.ExitException();
    }
}
