// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.isVector;
import com.amazon.ion.IonList;

final class IterateListProc
    extends Procedure
{
    IterateListProc()
    {
        super("Returns an iterator that produces the elements of a LIST.",
              "list");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        if (isVector(eval, args[0]))
        {
            return Iterators.iterate(FusionVector.unsafeJavaIterate(args[0]));
        }

        IonList ionList = checkIonListArg(0, args);

        return Iterators.iterateIonSequence(ionList);
    }
}
