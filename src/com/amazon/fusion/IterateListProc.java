// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.isVector;
import com.amazon.ion.IonList;

final class IterateListProc
    extends Procedure1
{
    IterateListProc()
    {
        super("Returns an iterator that produces the elements of a LIST.",
              "list");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        if (isVector(eval, arg))
        {
            return Iterators.iterate(FusionVector.unsafeJavaIterate(arg));
        }

        IonList ionList = checkIonListArg(0, arg);

        return Iterators.iterateIonSequence(ionList);
    }
}
