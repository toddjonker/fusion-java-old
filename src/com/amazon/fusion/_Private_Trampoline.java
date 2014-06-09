// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * NOT FOR APPLICATION USE
 */
public class _Private_Trampoline
{
    private _Private_Trampoline() {}


    public static void setDocumenting(FusionRuntimeBuilder rb,
                                      boolean documenting)
    {
        rb.setDocumenting(documenting);
    }

    public static void setCoverageCollector(FusionRuntimeBuilder rb,
                                            _Private_CoverageCollector collector)
    {
        rb.setCoverageCollector(collector);
    }

    public static FusionException newFusionException(String message,
                                                     Throwable cause)
    {
        return new FusionException(message, cause);
    }


    public static void flushMetrics(FusionRuntime r)
        throws FusionException
    {
        StandardRuntime sr = (StandardRuntime) r;
        _Private_CoverageCollector c = sr.getGlobalState().myCoverageCollector;
        if (c != null)
        {
            c.flushMetrics();
        }
    }
}
