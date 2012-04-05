// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonInt;
import java.io.IOException;
import java.io.Writer;

/**
 *
 */
class EqualFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function '='\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(= NUM NUM)\n\n");
        out.write("Returns true if the arguments are equal integers, false otherwise.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonInt left = (IonInt) ((DomValue) args[0]).getDom();
        IonInt right = (IonInt) ((DomValue) args[1]).getDom();

        boolean result = left.longValue() == right.longValue();
        IonBool resultDom = left.getSystem().newBool(result);
        return new DomValue(resultDom);
    }
}
