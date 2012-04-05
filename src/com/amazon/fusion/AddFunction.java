// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
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
        out.write("(add LIST VALUE)\n\n");
        out.write("Adds the VALUE to the end of the LIST.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonList list = (IonList) ((DomValue) args[0]).getDom().clone();
        IonValue value = ((DomValue) args[1]).getDom();
        value = Evaluator.cloneIfContained(value);
        list.add(value);
        return new DomValue(list);
    }
}
