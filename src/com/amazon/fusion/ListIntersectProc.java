// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonValue;
import java.util.HashMap;
import java.util.Map;


/**
 * Performs intersection by constructing a list elements that
 * are contained in both input lists - in O(n) time
 */
final class ListIntersectProc
    extends Procedure
{
    ListIntersectProc()
    {
        super("Performs intersection of 2 lists");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2,args);

        IonList resultList = eval.getSystem().newEmptyList();

        IonList ionList1 = checkListArg(0, args);
        IonList ionList2 = checkListArg(1, args);

        Map<IonValue,Boolean> intersectMap = new HashMap<IonValue, Boolean>();

        for (IonValue ionVal : ionList1)
        {
            intersectMap.put(ionVal, false);
        }

        for (IonValue searchVal : ionList2)
        {
            if(intersectMap.get(searchVal) != null)
            {
                intersectMap.put(searchVal, true);
            }
        }

        for (Map.Entry<IonValue, Boolean> entry : intersectMap.entrySet())
        {
            if(entry.getValue())
            {
                resultList.add(entry.getKey().clone());
            }
        }

        return new DomValue(resultList);
    }
}
