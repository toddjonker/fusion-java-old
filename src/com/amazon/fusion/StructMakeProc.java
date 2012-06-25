// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;


/**
 *
 */
final class StructMakeProc
    extends Procedure
{
    StructMakeProc()
    {
        //    "                                                                               |
        super("Constructs a struct from a sequence of alternating strings and Ion Values");
    }

    void checkArityEven(FusionValue... args)
        throws FusionException
    {
        if ((args.length % 2) == 1)
        {
            throw contractFailure("Expected: "+Integer.toString(args.length+1)+
                                  " or "+ Integer.toString(args.length-1)+
                                  "; observed: "+Integer.toString(args.length));
        }
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityEven(args);

        IonStruct result = eval.getSystem().newEmptyStruct().clone();

        for (int i = 0; i < args.length; i++)
        {
            String key = checkTextArg(i, args);
            i++;
            IonValue value = checkIonArg(i, args);
            result.put(key, value.clone());
        }

        return new DomValue(result);
    }
}
