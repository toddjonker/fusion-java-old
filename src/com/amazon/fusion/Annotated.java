// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * A Fusion value that may have annotations.
 */
interface Annotated
{
    /**
     * Gets the annotations as Java strings.
     *
     * @return not null, but possibly empty.
     */
    String[] annotationsAsJavaStrings()
        throws FusionException;
}
