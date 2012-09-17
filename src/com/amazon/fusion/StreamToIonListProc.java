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
        super("Converts a STREAM to an ion list.",
              "stream");
    }

    static IonList transform(Evaluator eval, Stream stream)
        throws ContractFailure, FusionException
    {
        IonList ionList = eval.getSystem().newEmptyList();
        while (stream.hasNext())
        {
            Object fv = stream.next();
            IonValue iv = FusionValue.toIonValue(fv);
            IonValue iv2 = FusionUtils.cloneIfContained(iv);
            ionList.add(iv2);
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
        return new DomValue(ionList);
    }
}
