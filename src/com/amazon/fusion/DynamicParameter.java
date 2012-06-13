// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonString;

/**
 *
 */
final class DynamicParameter
    extends Procedure
{
    private final FusionValue myInitialValue;


    public DynamicParameter(FusionValue initialValue)
    {
        super(null);
        myInitialValue = initialValue;
    }


    FusionValue getInitialValue()
    {
        return myInitialValue;
    }


    FusionValue currentValue(Evaluator eval)
    {
        FusionValue result = eval.firstContinuationMark(this);
        return (result == null ? myInitialValue : result);
    }


    String asString(Evaluator eval)
        throws FusionException
    {
        String result = null;
        FusionValue value = currentValue(eval);
        if (value != null && value != UNDEF)
        {
            // TODO error handling
            result = ((IonString) value.ionValue()).stringValue();
        }
        return result;
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(0, args);
        return currentValue(eval);
    }
}
