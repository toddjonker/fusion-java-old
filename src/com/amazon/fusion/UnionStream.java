// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Class to express union'd streams
 */
class UnionStream
    extends Stream
{

    private final Stream mySource1;
    private final Stream mySource2;

    public UnionStream(Stream source1, Stream source2)
    {
        this.mySource1 = source1;
        this.mySource2 = source2;
    }

    @Override
    boolean hasNext()
        throws FusionException
    {
        return (mySource1.hasNext() || mySource2.hasNext());
    }

    @Override
    FusionValue next()
        throws ContractFailure, FusionException
    {

        if (mySource1.hasNext())
        {
            return mySource1.next();
        } else
        {
            if (mySource2.hasNext())
            {
                return mySource2.next();
            }
        }

        throw new ContractFailure("No more objects left to fetch in stream.");
    }
}
