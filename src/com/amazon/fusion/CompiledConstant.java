// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class CompiledConstant
    implements CompiledForm
{
    private final Object myValue;

    CompiledConstant(Object value)
    {
        myValue = value;
    }

    @Override
    public Object doEval(Evaluator eval, Store store)
        throws FusionException
    {
        return myValue;
    }
}
