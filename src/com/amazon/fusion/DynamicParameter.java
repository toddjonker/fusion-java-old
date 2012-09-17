// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


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
        Object value = currentValue(eval);
        return FusionValue.asJavaString(value);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);
        return currentValue(eval);
    }
}
