// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.cloneIfContained;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;

/**
 *
 */
class AddProc
    extends Procedure
{
    AddProc()
    {
        //    "                                                                               |
        super("Adds the Ion DATUM to the end of the SEQUENCE (Ion list or sexp).",
              "sequence", "datum");
    }


    static void invoke(IonSequence seq, IonValue value)
    {
        value = cloneIfContained(value);
        seq.add(value);
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);
        IonSequence seq = checkSequenceArg(0, args);
        IonValue value = checkIonArg(1, args);

        invoke(seq, value);

        return args[0]; // Return the original FusionValue, no need to rewrap
    }
}
