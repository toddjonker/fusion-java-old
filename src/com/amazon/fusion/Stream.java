// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 * Base class for Fusion streams.
 * <p>
 * These are not equivalent to Racket streams, since those are functional and
 * these are stateful.
 */
abstract class Stream
    extends FusionValue
{
    abstract boolean hasNext() throws FusionException;

    abstract FusionValue next() throws ContractFailure, FusionException;

    @Override
    public void write(Appendable out) throws IOException
    {
        out.append("/* stream */");
    }

}
