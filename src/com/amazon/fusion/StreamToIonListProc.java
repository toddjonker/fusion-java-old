// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;

/**
 * Converts a stream to an ion list
 */
final class StreamToIonListProc
    extends Procedure
{
    StreamToIonListProc()
    {
        super("Converts a STREAM to an ion list.",
              "stream");
    }

    static IonList transform(Evaluator eval, Stream stream)
        throws ContractFailure, FusionException
    {
        IonSystem system = eval.getSystem();
        IonList ionList = system.newEmptyList();
        while (stream.hasNext())
        {
            Object fv = stream.next();
            IonValue iv = FusionValue.copyToIonValue(fv, system);
            ionList.add(iv);
        }
        return ionList;
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        // check if first arg is of stream class
        Stream source = checkStreamArg(0,args);
        IonList ionList = transform(eval, source);
        return eval.inject(ionList);
    }
}
