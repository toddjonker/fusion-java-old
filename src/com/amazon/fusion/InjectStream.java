// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Stateful stream for injection
 */
class InjectStream
    extends Stream
{
    private FusionValue mySource;

    InjectStream(FusionValue source)
    {
        this.mySource = source;
    }

    @Override
    boolean hasNext()
    {
        return (mySource != null);
    }

    @Override
    FusionValue next()
        throws ContractFailure
    {
        FusionValue result = mySource;
        if (result == null)
        {
            throw new ContractFailure("Out of arguments to fetch from stream");
        }
        mySource = null;
        return result;
    }

}
