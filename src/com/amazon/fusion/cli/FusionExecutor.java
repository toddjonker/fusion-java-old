// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

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
    final FusionRuntime myRuntime;


    FusionExecutor(boolean documenting)
    {
        FusionRuntimeBuilder builder = FusionRuntimeBuilder.standard();

        _Private_Trampoline.setDocumenting(builder, documenting);

        myRuntime = builder.build();
    }
}
