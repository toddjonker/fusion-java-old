// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
final class DynamicParameter
    extends FunctionValue
{
    private final FusionValue myInitialValue;


    DynamicParameter(FusionValue initialValue)
    {
        super(null);
        myInitialValue = initialValue;
    }


    FusionValue getInitialValue()
    {
        return myInitialValue;
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(0, args);
        FusionValue result = eval.firstContinuationMark(this);
        return (result == null ? myInitialValue : result);
    }
}
