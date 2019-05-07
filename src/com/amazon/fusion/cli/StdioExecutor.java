// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.cli.Command.Executor;
import java.io.PrintStream;

/**
 * Base {@link Executor} that's configured with the standard output streams.
 */
abstract class StdioExecutor
    implements Executor
{
    private final PrintStream myStdout;
    private final PrintStream myStderr;

    StdioExecutor(GlobalOptions globals)
    {
        myStdout = globals.stdout();
        myStderr = globals.stderr();
    }

    PrintStream stdout()
    {
        return myStdout;
    }

    PrintStream stderr()
    {
        return myStderr;
    }
}
