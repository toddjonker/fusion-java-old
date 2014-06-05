// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.stringToJavaString;
import java.util.List;


/**
 *
 */
class DynamicParameter
    extends Procedure0
{
    private final Object myInitialValue;


    public DynamicParameter(Object initialValue)
    {
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
        return stringToJavaString(eval, value);
    }


    @Override
    Object doApply(Evaluator eval)
        throws FusionException
    {
        return currentValue(eval);
    }
}
