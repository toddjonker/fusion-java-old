// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonValue;

/**
 * Converts a stream to an ion list
 */
final class StreamToIonListProc
    extends Procedure
{
    StreamToIonListProc()
    {
        super("Converts a stream to an ion list");
    }

    static IonList transform(Evaluator eval, Stream stream)
        throws ContractFailure, FusionException
    {
        IonList ionList = eval.getSystem().newEmptyList();
        while (stream.hasNext())
        {
            FusionValue fv = stream.next();
            IonValue iv = fv.ionValue();
            IonValue iv2 = FusionUtils.cloneIfContained(iv);
            ionList.add(iv2);
        }
        return ionList;
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        // check if first arg is of stream class
        Stream source = checkStreamArg(0,args);
        IonList ionList = transform(eval, source);
        return new DomValue(ionList);
    }

}
