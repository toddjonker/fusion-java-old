// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.junit;

import static java.nio.charset.StandardCharsets.UTF_8;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import org.junit.After;
import org.junit.Before;


public class StdioTestCase
{
    private ByteArrayOutputStream myStdoutBytes;
    private ByteArrayOutputStream myStderrBytes;

    private PrintStream myStdout;
    private PrintStream myStderr;


    @Before
    public void setUpStdio()
    {
        myStdoutBytes = new ByteArrayOutputStream();
        myStderrBytes = new ByteArrayOutputStream();

        myStdout = new PrintStream(myStdoutBytes);
        myStderr = new PrintStream(myStderrBytes);
    }

    @After
    public void tearDownStdio()
    {
        myStdoutBytes = null;
        myStderrBytes = null;
        myStdout = null;
        myStderr = null;
    }


    protected PrintStream stdout()
    {
        return myStdout;
    }

    protected PrintStream stderr()
    {
        return myStderr;
    }


    protected String stdoutToString()
    {
        return toString(myStdoutBytes);
    }

    protected String stderrToString()
    {
        return toString(myStderrBytes);
    }


    private String toString(ByteArrayOutputStream bytes)
    {
        try
        {
            return bytes.toString(UTF_8.name());
        }
        catch (UnsupportedEncodingException e)
        {
            throw new AssertionError(e);
        }
    }
}
