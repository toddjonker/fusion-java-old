// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.List;


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


    @SuppressWarnings("unchecked")
    <T> T currentValue(Evaluator eval)
    {
        Object result = eval.firstContinuationMark(this);
        return (T) (result == null ? myInitialValue : result);
    }


    <T> List<T> allValues(Evaluator eval)
    {
        List<T> marks = (List<T>) eval.continuationMarks(this);

        // TODO add initial value?

        return marks;
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
