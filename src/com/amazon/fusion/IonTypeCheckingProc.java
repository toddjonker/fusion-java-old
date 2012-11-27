// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonType;
import com.amazon.ion.IonValue;

/**
 * Implements some of the {@code is_TYPE} procedures for the Ion types.
 */
final class IonTypeCheckingProc
    extends Procedure1
{
    private final IonType myType;

    IonTypeCheckingProc(IonType type)
    {
        //    "                                                                               |
        super("Determines whether a `value` is of type "
                 + type.name().toLowerCase()
                 + ", returning `true` or `false`.",
              "value");

        myType = type;
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        IonValue iv = castToIonValueMaybe(arg);
        boolean result = (iv != null && iv.getType() == myType);
        return eval.newBool(result);
    }
}
