// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;
import java.io.IOException;
import java.io.Writer;

/**
 *
 */
class SizeFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function 'size'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(size LIST)\n\n");
        out.write("Returns the number of child elements contained in the LIST.\n");
        out.write("The size of the null.list is zero.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonList list = (IonList) ((DomValue) args[0]).getDom();
        IonInt result = list.getSystem().newInt(list.size());
        return new DomValue(result);
    }
}
