// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import java.io.IOException;

/**
 * The Fusion command-line interface, just here to provide a {@link #main} method.
 */
public final class Cli
{
    private Cli() {}


    public static void main(String[] args) throws IOException
    {
        int errorCode;

        try
        {
            errorCode = CommandFactory.executeCommandLine(args);
        }
        catch (Throwable e)
        {
            errorCode = 1;
            e.printStackTrace(System.err);
        }

        System.out.flush();
        System.err.flush();

        if (errorCode != 0)
        {
            System.exit(errorCode);
        }
    }
}
