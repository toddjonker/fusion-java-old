// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Represents conditions raised within Fusion code, as opposed to failures
 * within the interpreter implementation.
 */
@SuppressWarnings("serial")
public class FusionException
    extends Exception
{
    FusionException(String message)
    {
        super(message);
    }

    FusionException(String message, Throwable cause)
    {
        super(message, cause);
    }

    FusionException(Throwable cause)
    {
        super(cause.getMessage(), cause);
    }
}
