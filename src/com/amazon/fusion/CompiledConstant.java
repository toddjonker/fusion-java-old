// Copyright (c) 2012-2016 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class CompiledConstant
    implements CompiledForm
{
    private final Object myValue;

    CompiledConstant(Object value)
    {
        myValue = value;
    }

    Object getValue()
    {
        return myValue;
    }

    @Override
    public Object doEval(Evaluator eval, Store store)
        throws FusionException
    {
        return myValue;
    }
}
