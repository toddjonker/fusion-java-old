// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.immutableList;
import static com.amazon.fusion.FusionSymbol.makeSymbol;
import static com.amazon.fusion.FusionUtils.EMPTY_OBJECT_ARRAY;

final class TypeAnnotationsProc
    extends Procedure1
{
    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        String[] anns = annotationsAsJavaStrings(eval, arg);

        Object[] result = EMPTY_OBJECT_ARRAY;
        int length = anns.length;
        if (length != 0)
        {
            result = new Object[length];
            for (int i = 0; i < length; i++)
            {
                result[i] = makeSymbol(eval, anns[i]);
            }
        }

        // Returning immutable list allows us to return a shared structure
        // when possible, avoiding copies.
        return immutableList(eval, result);
    }
}
