// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Class to express empty streams
 */
final class EmptyStream
    extends Stream
{
    @Override
    boolean hasNext()
    {
        return false;
    }

    @Override
    FusionValue next()
        throws ContractFailure
    {
        throw new ContractFailure("No objects available in an empty stream");
    }
}
