package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonValue;

final class GetFromListAtIndexProc
    extends Procedure
{
    // (get [list] idx)
    GetFromListAtIndexProc()
    {
        //    "                                                                               |
        super("Returns the element at index in list");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        IonList ionList = checkListArg(0, args);

        int index = (int)checkLongArg(1, args);

        IonValue ionValue = ionList.get(index);

        return new DomValue(ionValue); // Don't need to re-wrap the input struct
    }
}
