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
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonList ionList = checkListArg(0, args);

        int index = checkIntArg(1, args);

        IonValue ionValue = ionList.get(index);

        return eval.inject(ionValue);
    }
}
