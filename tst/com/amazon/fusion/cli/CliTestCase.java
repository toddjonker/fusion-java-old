// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.junit.Assert.assertEquals;
import com.amazon.fusion.junit.StdioTestCase;
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
        int errorCode = myCommandFactory.executeCommandLine(commandLine);

        assertEquals("error code", expectedErrorCode, errorCode);

        stdoutText = stdoutToString();
        stderrText = stderrToString();
    }
}
