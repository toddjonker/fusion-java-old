// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonType;

/**
 * Implements all the {@code is_TYPE} procedures for the Ion types.
 */
final class IonTypeCheckingProc
    extends Procedure1
{
    private final IonType myType;

    IonTypeCheckingProc(IonType type)
    {
        //    "                                                                               |
        super("Determines whether a VALUE is an Ion "
                 + type.name().toLowerCase()
                 + ", returning true or false.",
              "value");

        myType = type;
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        boolean result = (FusionValue.ionType(arg) == myType);
        return eval.newBool(result);
    }
}
