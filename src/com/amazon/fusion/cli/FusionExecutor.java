// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion._Private_Trampoline;
import com.amazon.fusion.cli.Command.Executor;

/**
 *
 */
abstract class FusionExecutor
    implements Executor
{
    private final boolean isDocumenting;
    private FusionRuntime myRuntime;


    FusionExecutor(boolean documenting)
    {
        isDocumenting = documenting;
    }


    FusionRuntimeBuilder runtimeBuilder()
    {
        FusionRuntimeBuilder builder = FusionRuntimeBuilder.standard();

        _Private_Trampoline.setDocumenting(builder, isDocumenting);

        return builder;
    }


    FusionRuntime runtime()
        throws FusionException
    {
        if (myRuntime == null)
        {
            FusionRuntimeBuilder builder = runtimeBuilder();
            myRuntime = builder.build();
        }
        return myRuntime;
    }
}
