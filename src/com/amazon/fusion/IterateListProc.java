// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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

        // check if first arg is of IonList class
        IonList ionList = checkListArg(0, args);

        return Iterators.iterateIonSequence(ionList);
    }
}
