// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWrite;
import java.io.IOException;

/**
 * Indicates a failure to convert a Fusion value into Ion.
 */
@SuppressWarnings("serial")
class IonizeFailure
    extends ContractException
{
    private final Object myUnIonizableValue;

    IonizeFailure(Object unIonizableValue)
    {
        super(null);

        myUnIonizableValue = unIonizableValue;
    }


    @Override
    void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        out.append("Cannot ionize non-Ionizable data: ");
        safeWrite(eval, out, myUnIonizableValue);
    }
}
