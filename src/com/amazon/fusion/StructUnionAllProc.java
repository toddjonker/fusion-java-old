// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;
import java.util.Iterator;

final class StructUnionAllProc
    extends Procedure
{
    StructUnionAllProc()
    {
        //    "                                                                               |
        super("Constructs a struct by performing a union of up to two structs");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        // even # of args = list of structs?

        IonStruct result = eval.getSystem().newEmptyStruct();

        for (int i = 0; i < args.length; i++)
        {
            IonStruct struct1 = checkStructArg(i, args);

            Iterator<IonValue> iterator1 = struct1.iterator();

            while(iterator1.hasNext())
            {
                IonValue ionValue = iterator1.next();
                String fieldName = ionValue.getFieldName();
                result.put(fieldName, ionValue.clone());
            }
        }

        return eval.inject(result);
    }
}
