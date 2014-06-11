// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionRuntimeBuilder;
import java.io.File;

/**
 *
 */
class GlobalOptions
{
    String myBootstrapPath;

    public void setBootstrapRepository(String path)
    {
        myBootstrapPath = path;
    }


    FusionRuntimeBuilder runtimeBuilder()
        throws UsageException
    {
        FusionRuntimeBuilder builder = FusionRuntimeBuilder.standard();

        if (myBootstrapPath != null)
        {
            File dir = new File(myBootstrapPath);

            try
            {
                builder.setBootstrapRepository(dir);
            }
            catch (IllegalArgumentException e)
            {
                String message = "bootstrapRepository: " + e.getMessage();
                throw new UsageException(message);
            }
        }

        return builder;
    }
}
