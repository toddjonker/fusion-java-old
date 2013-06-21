// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionValue.evaluator;
import com.amazon.ion.IonBool;
import com.amazon.ion.IonValue;

/**
 * Utilities for Fusion {@code bool} values.
 */
public final class FusionBool
{
    private FusionBool() {}


    public static boolean isBool(TopLevel top, Object value)
        throws FusionException
    {
        return isBool(evaluator(top), value);
    }

    static boolean isBool(Evaluator eval, Object value)
        throws FusionException
    {
        IonValue iv = FusionValue.castToIonValueMaybe(value);
        return (iv instanceof IonBool);
    }


    public static boolean isTrue(TopLevel top, Object value)
        throws FusionException
    {
        return isTrue(evaluator(top), value);
    }

    static boolean isTrue(Evaluator eval, Object value)
        throws FusionException
    {
        IonValue iv = FusionValue.castToIonValueMaybe(value);
        if (iv instanceof IonBool && ! iv.isNullValue())
        {
            IonBool bv = (IonBool) iv;
            return bv.booleanValue();
        }

        return false;
    }


    public static boolean isFalse(TopLevel top, Object value)
        throws FusionException
    {
        return isFalse(evaluator(top), value);
    }

    static boolean isFalse(Evaluator eval, Object value)
        throws FusionException
    {
        IonValue iv = FusionValue.castToIonValueMaybe(value);
        if (iv instanceof IonBool && ! iv.isNullValue())
        {
            IonBool bv = (IonBool) iv;
            return ! bv.booleanValue();
        }

        return false;
    }
}
