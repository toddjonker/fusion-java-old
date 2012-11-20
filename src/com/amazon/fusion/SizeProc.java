// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.isVector;
import static com.amazon.fusion.FusionVector.unsafeVectorSize;
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
        if (isVector(eval, arg))
        {
            size = unsafeVectorSize(eval, arg);
        }
        else
        {
            IonContainer c = checkIonContainerArg(0, arg);
            size = c.size();
        }

        return eval.newInt(size);
    }
}
