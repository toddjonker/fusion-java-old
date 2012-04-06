// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Represents conditions raised within Fusion code, as opposed to failures
 * within the interpreter implementation.
 */
public class FusionException
    extends Exception
{
    private static final long serialVersionUID = 1L;

    FusionException(String message)
    {
        super(message);
    }
}
