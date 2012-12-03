// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionCollection.isCollection;
import static com.amazon.fusion.FusionCollection.unsafeCollectionSize;
import com.amazon.ion.IonContainer;

final class SizeProc
    extends Procedure1
{
    SizeProc()
    {
        //    "                                                                               |
        super("Returns the number of elements in the `collection`.\n" +
              "The size of `null.list` (_etc._) is zero.",
              "collection");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        int size;
        if (isCollection(eval, arg))
        {
            size = unsafeCollectionSize(eval, arg);
        }
        else
        {
            IonContainer c = checkIonContainerArg(0, arg);
            size = c.size();
        }

        return eval.newInt(size);
    }
}
