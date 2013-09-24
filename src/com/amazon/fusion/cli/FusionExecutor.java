// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion._Private_Trampoline;
import com.amazon.fusion.cli.Command.Executor;
import java.io.File;

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

        String property = "com.amazon.fusion.BootstrapRepository";
        String bootstrap = System.getProperty(property);
        if (bootstrap != null)
        {
            File file = new File(bootstrap);
            if (! file.isDirectory())
            {
                String message =
                    "Value of system property " + property +
                    " is not a directory: " + bootstrap;
                throw new IllegalArgumentException(message);
            }

            if (! file.isAbsolute())
            {
                file = file.getAbsoluteFile();
            }

            builder.addRepositoryDirectory(file);
        }

        _Private_Trampoline.setDocumenting(builder, documenting);

        myRuntime = builder.build();
    }
}
