// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;

final class IterateListProc
    extends Procedure1
{
    IterateListProc()
    {
        super("Returns an iterator that produces the elements of an Ion LIST_OR_SEXP.",
              "list_or_sexp");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        IonList ionList = checkIonListArg(0, arg);

        return Iterators.iterateIonSequence(ionList);
    }
}
