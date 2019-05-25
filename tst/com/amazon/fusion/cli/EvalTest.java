// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import org.junit.Test;

public class EvalTest
    extends CliTestCase
{
    @Test
    public void testVoidResult()
        throws Exception
    {
        run(0, "eval", "(void)");

        assertThat(stdoutText, isEmptyString());
        assertThat(stderrText, isEmptyString());
    }

    @Test
    public void testSingleResult()
        throws Exception
    {
        run(0, "eval", "true");

        assertEquals("true\n", stdoutText);
        assertThat(stderrText, isEmptyString());
    }

    @Test
    public void testMultipleResults()
        throws Exception
    {
        run(0, "eval", "(values 1 2)");

        assertEquals("1\n2\n", stdoutText);
        assertThat(stderrText, isEmptyString());
    }

    @Test
    public void testDisplayOutput()
        throws Exception
    {
        run(0, "eval", "(begin (displayln '''my output''') (quote result))");

        assertEquals("my output\nresult\n", stdoutText);
        assertThat(stderrText, isEmptyString());
    }
}
