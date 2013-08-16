// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * NOT FOR APPLICATION USE
 */
public class _Private_Trampoline
{
    private _Private_Trampoline() {}

    public static FusionException newFusionException(String message,
                                                     Throwable cause)
    {
        return new FusionException(message, cause);
    }
}
