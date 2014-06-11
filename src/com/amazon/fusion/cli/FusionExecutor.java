// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion.cli.Command.Executor;

/**
 *
 */
abstract class FusionExecutor
    implements Executor
{
    private final GlobalOptions myGlobalOptions;
    private FusionRuntime myRuntime;


    FusionExecutor(GlobalOptions globals)
    {
        myGlobalOptions = globals;
    }


    FusionRuntimeBuilder runtimeBuilder()
        throws UsageException
    {
        return myGlobalOptions.runtimeBuilder();
    }


    FusionRuntime runtime()
        throws FusionException, UsageException
    {
        if (myRuntime == null)
        {
            FusionRuntimeBuilder builder = runtimeBuilder();
            myRuntime = builder.build();
        }
        return myRuntime;
    }
}
