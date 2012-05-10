// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonContainer;
import com.amazon.ion.IonInt;

/**
 *
 */
class SizeProc
    extends Procedure
{
    SizeProc()
    {
        //    "                                                                               |
        super("Returns the number of child elements contained in the CONTAINER.\n" +
              "The size of null.list (etc) is zero.",
              "container");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        IonContainer c = checkContainerArg(0, args);
        IonInt result = c.getSystem().newInt(c.size());
        return new DomValue(result);
    }
}
