// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonInt;

/**
 *
 */
class EqualProc
    extends Procedure
{
    EqualProc()
    {
        //    "                                                                               |
        super("Returns true if the arguments are equal integers, false otherwise.",
              "int", "int");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonInt left = (IonInt) ((DomValue) args[0]).getDom();
        IonInt right = (IonInt) ((DomValue) args[1]).getDom();

        boolean result = left.longValue() == right.longValue();
        IonBool resultDom = left.getSystem().newBool(result);
        return new DomValue(resultDom);
    }
}
