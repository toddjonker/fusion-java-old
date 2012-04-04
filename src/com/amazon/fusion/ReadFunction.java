// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;
import java.util.Iterator;

/**
 *
 */
class ReadFunction
    extends FunctionValue
{
    private Iterator<IonValue> myInputValues;

    @Override
    void print(Writer out) throws IOException
    {
        out.write("// Function 'read'\n");
    }

    @Override
    void printDoc(Writer out)
        throws IOException
    {
        out.write("(read)\n\n");
        out.write("Reads an Ion value from the standard input stream.\n" +
                  "Returns undef when there's no more data; use (is_undef) to\n" +
                  "check for it.");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        if (myInputValues == null)
        {
            myInputValues = eval.getSystem().iterate(System.in);
        }

        FusionValue result;
        if (myInputValues.hasNext())
        {
            IonValue v = myInputValues.next();
            result = new DomValue(v);
        }
        else
        {
            result = FusionValue.UNDEF;
        }

        return result;
    }
}
