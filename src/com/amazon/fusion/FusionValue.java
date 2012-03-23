// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;
import java.io.IOException;
import java.io.Writer;


abstract class FusionValue
{
    IonValue getDom()
    {
        return null;
    }

    abstract void print(Writer out)
        throws IOException;

    abstract FusionValue invoke(Evaluator eval,
                                Environment context,
                                IonSexp expr);
}
