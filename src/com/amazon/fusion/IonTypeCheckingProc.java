// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonType;

/**
 * Implements all the {@code is_TYPE} procedures for the Ion types.
 */
final class IonTypeCheckingProc
    extends Procedure
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
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(args);
        Object fv = args[0];
        boolean result = (FusionValue.ionType(fv) == myType);
        return eval.newBool(result);
    }
}
