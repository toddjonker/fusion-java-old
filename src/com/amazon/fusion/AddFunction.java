// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;

/**
 *
 */
class AddFunction
    extends FunctionValue
{
    AddFunction()
    {
        //    "                                                                               |
        super("Adds the Ion DATUM to the end of the SEQUENCE (Ion list or sexp).",
              "sequence", "datum");
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
