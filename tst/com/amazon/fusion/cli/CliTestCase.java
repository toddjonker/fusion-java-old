// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.junit.Assert.assertEquals;
import com.amazon.fusion.junit.StdioTestCase;
import java.util.ArrayList;
import java.util.Arrays;
import org.junit.After;
import org.junit.Before;

public class CliTestCase
    extends StdioTestCase
{
    private CommandFactory myCommandFactory;

    protected String stdoutText;
    protected String stderrText;


    @Before
    public void setupCli()
    {
        myCommandFactory = new CommandFactory(stdout(), stderr());
    }

    @After
    public void tearDownCli()
    {
        myCommandFactory = null;
    }


    void run(String... commandLine)
        throws Exception
    {
        run(0, commandLine);
    }

    void run(int expectedErrorCode, String... commandLine)
        throws Exception
    {
        commandLine = prependGlobalOptions(commandLine);

        int errorCode = myCommandFactory.executeCommandLine(commandLine);

        assertEquals("error code", expectedErrorCode, errorCode);

        stdoutText = stdoutToString();
        stderrText = stderrToString();
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
