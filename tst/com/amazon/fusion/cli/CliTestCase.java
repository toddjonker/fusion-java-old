// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.junit.Assert.assertEquals;
import com.amazon.fusion.junit.StdioTestCase;
import java.util.ArrayList;
import java.util.Arrays;
import org.junit.After;

public class CliTestCase
    extends StdioTestCase
{
    protected String stdoutText;
    protected String stderrText;


    @After
    public void tearDownCli()
    {
        stdoutText = null;
        stderrText = null;
    }


    void run(String... commandLine)
        throws Exception
    {
        run(0, commandLine);
    }

    void run(int expectedErrorCode, String... commandLine)
        throws Exception
    {
        int errorCode = execute(commandLine);

        assertEquals("error code", expectedErrorCode, errorCode);

        stdoutText = stdoutToString();
        stderrText = stderrToString();
    }

    private int execute(String... commandLine)
        throws Exception
    {
        commandLine = prependGlobalOptions(commandLine);

        CommandFactory cf = new CommandFactory(stdin(), stdout(), stderr());
        return cf.executeCommandLine(commandLine);
    }

    private String[] prependGlobalOptions(String[] commandLine)
    {
        ArrayList<String> join = new ArrayList<>();

        join.add("--bootstrapRepository");
        join.add("fusion");

        join.addAll(Arrays.asList(commandLine));
        return join.toArray(new String[0]);
    }
}
