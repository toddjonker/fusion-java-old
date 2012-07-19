// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import java.io.IOException;

/**
 * The Fusion command-line interface, just here to provide a {@link #main} method.
 */
public final class Cli
{
    public static void main(String[] args) throws IOException
    {
        boolean success = CommandFactory.executeCommandLine(args);

        System.out.flush();
        System.err.flush();

        if (! success)
        {
            System.exit(1);
        }
    }
}
