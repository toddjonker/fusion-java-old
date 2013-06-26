// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 *
 */
class DynamicParameter
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


    <T> T currentValue(Evaluator eval)
    {
        Object result = eval.firstContinuationMark(this);
        return (T) (result == null ? myInitialValue : result);
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
