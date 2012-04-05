// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;

/**
 *
 */
class AddFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function 'add'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(add SEQUENCE VALUE)\n\n");
        out.write("Adds the VALUE to the end of the SEQUENCE (Ion list or sexp).\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonSequence seq = DomValue.assumeSequence(args[0]);
        IonValue value = ((DomValue) args[1]).getDom();
        value = Evaluator.cloneIfContained(value);
        seq.add(value);
        return args[0];
    }
}
