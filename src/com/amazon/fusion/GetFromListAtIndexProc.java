// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonValue;

final class GetFromListAtIndexProc
    extends Procedure
{
    GetFromListAtIndexProc()
    {
        //    "                                                                               |
        super("Returns the element at INDEX in LIST",
              "list", "index");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        IonList ionList = checkListArg(0, args);

        int index = (int)checkLongArg(1, args);

        IonValue ionValue = ionList.get(index);

        return new DomValue(ionValue);
    }
}
