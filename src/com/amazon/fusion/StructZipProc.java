// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionPrint.safeWriteToString;
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

        Iterator<?> fieldIterator = unsafeJavaIterate(eval, fieldList);
        Iterator<?> valueIterator = unsafeJavaIterate(eval, valueList);

        while (fieldIterator.hasNext() && valueIterator.hasNext())
        {
            Object nameObj = fieldIterator.next();
            String name = FusionValue.asJavaString(nameObj);
            if (name == null)
            {
                throw new ResultFailure(identify(),
                                        "string in field-name sequence",
                                        safeWriteToString(eval, nameObj));
            }

            Object valueObj = valueIterator.next();
            IonValue value = copyToIonValue(valueObj, eval.getSystem());
            result.add(name, value);
        }

        return eval.inject(result);
    }
}
