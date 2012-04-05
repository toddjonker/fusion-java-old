// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;

/**
 *
 */
class IsNullFunction
    extends FunctionValue
{
    @Override
    void display(Writer out) throws IOException
    {
        out.write("// Function 'is_null'\n");
    }

    @Override
    void printHelp(Writer out)
        throws IOException
    {
        out.write("(is_null VALUE)\n\n");
        out.write("Returns true when VALUE is any Ion null, false otherwise.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        boolean isNull;
        FusionValue arg = args[0];
        if (arg instanceof DomValue)
        {
            IonValue value = ((DomValue) arg).getDom();
            isNull = value.isNullValue();
        }
        else
        {
            isNull = false;
        }
        IonValue result = eval.getSystem().newBool(isNull);
        return new DomValue(result);
    }
}
