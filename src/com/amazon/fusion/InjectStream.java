// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Stateful stream for injection
 */
final class InjectStream
    extends Stream
{
    private Object mySource;

    InjectStream(Object source)
    {
        this.mySource = source;
    }

    @Override
    boolean hasNext()
    {
        return (mySource != null);
    }

    @Override
    Object next()
        throws ContractFailure
    {
        Object result = mySource;
        if (result == null)
        {
            throw new ContractFailure("Out of arguments to fetch from stream");
        }
        mySource = null;
        return result;
    }

}
