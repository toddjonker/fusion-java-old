// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionPrint.safeWrite;
import java.io.IOException;

/**
 *
 */
@SuppressWarnings("serial")
class IonizeFailure
    extends ContractFailure
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
