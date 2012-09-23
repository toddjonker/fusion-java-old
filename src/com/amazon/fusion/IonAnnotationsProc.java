// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;

final class IonAnnotationsProc
    extends Procedure
{
    IonAnnotationsProc()
    {
        //    "                                                                               |
        super("Returns a non-null list of strings containing the user type annotations on the\n" +
              "VALUE.",
              "value");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        IonValue value = checkIonArg(0, args);

        IonSystem system = eval.getSystem();
        IonList result = system.newEmptyList();

        String[] anns = value.getTypeAnnotations();
        for (String ann : anns)
        {
            result.add(system.newString(ann));
        }

        return eval.inject(result);
    }
}
