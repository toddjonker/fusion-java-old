// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.unsafeJavaIterate;
import static com.amazon.fusion.FusionStruct.immutableStruct;
import static com.amazon.fusion.FusionStruct.structImplAdd;
import static com.amazon.fusion.FusionWrite.safeWriteToString;
import java.util.HashMap;
import java.util.Iterator;

final class StructZipProc
    extends Procedure2
{
    StructZipProc()
    {
        //    "                                                                               |
        super("Constructs a struct from a list of field names and a list of values.  The names\n" +
              "must be non-empty strings or symbols.",
              "names", "values");
    }


    @Override
    Object doApply(Evaluator eval, Object names, Object values)
        throws FusionException
    {
        checkListArg(eval, 0, names, values);
        checkListArg(eval, 1, names, values);

        Iterator<?> fieldIterator = unsafeJavaIterate(eval, names);
        Iterator<?> valueIterator = unsafeJavaIterate(eval, values);

        HashMap<String, Object> map = new HashMap<String, Object>();

        while (fieldIterator.hasNext() && valueIterator.hasNext())
        {
            Object nameObj = fieldIterator.next();
            String name = copyTextToJavaString(nameObj);
            if (name == null || name.isEmpty())
            {
                throw new ResultFailure(identify(),
                                        "non-empty string or symbol in field-name sequence",
                                        safeWriteToString(eval, nameObj));
            }

            Object valueObj = valueIterator.next();
            structImplAdd(map, name, valueObj);
        }

        return immutableStruct(map);
    }
}
