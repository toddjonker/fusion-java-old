// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;

/**
 *
 */
class IsUndefFunction
    extends FunctionValue
{
    @Override
    void print(Writer out) throws IOException
    {
        out.write("// Function 'is_undef'\n");
    }

    @Override
    void printDoc(Writer out)
        throws IOException
    {
        out.write("(is_undef EXPR)\n\n");
        out.write("Returns true when EXPR evaluates to the unique UNDEF object, false otherwise.\n");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        FusionValue arg = args[0];
        boolean isUndef = (arg == UNDEF);
        IonValue result = eval.getSystem().newBool(isUndef);
        return new DomValue(result);
    }
}
