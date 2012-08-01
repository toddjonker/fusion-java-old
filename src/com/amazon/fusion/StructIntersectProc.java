// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;
import java.util.Iterator;


/**
 * Performs intersection of struct - constructed with matching fields and values
 */
class StructIntersectProc
    extends Procedure
{
    StructIntersectProc()
    {
        super("Performs intersection of struct - constructed with matching fields and values");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2,args);

        IonStruct unionStruct = eval.getSystem().newEmptyStruct();

        IonStruct ionStruct1 = checkStructArg(0, args);
        IonStruct ionStruct2 = checkStructArg(1, args);

        Iterator<IonValue> iterator = ionStruct1.iterator();

        while (iterator.hasNext())
        {
            IonValue ionValue = iterator.next();
            String fieldName = ionValue.getFieldName();
            IonValue testIonValue = ionStruct2.get(fieldName);
            if (ionValue.equals(testIonValue))
            {
                unionStruct.add(fieldName,ionValue.clone());
            }
        }

        return new DomValue(unionStruct);
    }
}
