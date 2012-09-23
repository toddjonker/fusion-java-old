// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 *
 */
final class DynamicParameter
    extends Procedure
{
    private final Object myInitialValue;


    public DynamicParameter(Object initialValue)
    {
        super(null);
        myInitialValue = initialValue;
    }


    Object getInitialValue()
    {
        return myInitialValue;
    }


    Object currentValue(Evaluator eval)
    {
        Object result = eval.firstContinuationMark(this);
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
