// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonString;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;
import java.util.ListIterator;


/**
 *
 */
final class StructZipProc
    extends Procedure
{
    StructZipProc()
    {
        //    "                                                                               |
        super("Constructs a struct from a list of field names and a list of values");
    }
    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        // even # of args = list of structs?
        // checkArityEven(args);
        checkArityExact(2, args);

        IonStruct result = eval.getSystem().newEmptyStruct();

        IonList fieldList = checkListArg(0, args);
        IonList valueList = checkListArg(1, args);

        ListIterator<IonValue> fieldListIterator = fieldList.listIterator();
        ListIterator<IonValue> valueListIterator = valueList.listIterator();

        while (fieldListIterator.hasNext() && valueListIterator.hasNext())
        {
            IonValue ionField = fieldListIterator.next();
            IonValue ionValue = valueListIterator.next();
            if (ionField instanceof IonString)
            {
                String fieldName = ((IonString)ionField).stringValue();
                result.put(fieldName, ionValue.clone());
            } else
            {
                throw contractFailure("Expected: string"+
                                      "; observed: "+ionField.getType().toString());
            }
        }

        return new DomValue(result);
    }
}
