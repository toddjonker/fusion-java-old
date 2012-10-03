// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;
import java.util.Iterator;

final class StructZipProc
    extends Procedure
{
    StructZipProc()
    {
        //    "                                                                               |
        super("Constructs a struct from a list of field names and a list of values");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        // even # of args = list of structs?
        // checkArityEven(args);
        checkArityExact(2, args);

        IonStruct result = eval.getSystem().newEmptyStruct();

        Object fieldList = checkListArg(0, args);
        Object valueList = checkListArg(1, args);

        Iterator<?> fieldIterator = unsafeJavaIterate(fieldList);
        Iterator<?> valueIterator = unsafeJavaIterate(valueList);

        while (fieldIterator.hasNext() && valueIterator.hasNext())
        {
            Object nameObj = fieldIterator.next();
            String name = FusionValue.asJavaString(nameObj);
            if (name == null)
            {
                throw contractFailure("Expected: string; received: " +
                                      writeToString(nameObj));
            }

            Object valueObj = valueIterator.next();
            IonValue value = copyToIonValue(valueObj, eval.getSystem());
            result.put(name, value);
        }

        return eval.inject(result);
    }
}
