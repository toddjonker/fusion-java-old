// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.*;
import org.junit.Test;

public class RequireTest
    extends CliTestCase
{
    @Test
    public void testNoOutput()
        throws Exception
    {
        run(0, "require", "/fusion/struct");

        assertThat(stdoutText, isEmptyString());
        assertThat(stderrText, isEmptyString());
    }

    @Test
    public void testMissingImport()
        throws Exception
    {
        // `mutable_struct` is not provided by /fusion
        run(1, "eval", "(mutable_struct)");

        assertThat(stdoutText, isEmptyString());
        assertThat(stderrText, containsString("unbound identifier"));
        assertThat(stderrText, containsString("mutable_struct"));
    }

    @Test
    public void testCommandSequence()
        throws Exception
    {
        run(0, "require", "/fusion/struct", ";", "eval", "(mutable_struct)");

        assertEquals("{}\n", stdoutText);
        assertThat(stderrText, isEmptyString());
    }
}
