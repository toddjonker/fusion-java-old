// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;
import com.amazon.ion.IonList;

/**
 *
 */
class SizeFunction
    extends FunctionValue
{
    SizeFunction()
    {
        //    "                                                                               |
        super("Returns the number of child elements contained in the LIST.\n" +
              "The size of null.list is zero.",
              "list");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
    {
        IonList list = (IonList) ((DomValue) args[0]).getDom();
        IonInt result = list.getSystem().newInt(list.size());
        return new DomValue(result);
    }
}
