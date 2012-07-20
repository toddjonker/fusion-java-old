// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
public class FusionRuntimeBuilder
{
    private FusionRuntimeBuilder() { }

    public static FusionRuntimeBuilder standard()
    {
        return new FusionRuntimeBuilder();
    }

    public static FusionRuntime build()
    {
        return new StandardRuntime();
    }
}
