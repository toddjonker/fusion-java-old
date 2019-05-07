// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;
import org.junit.Test;

public class CommandFactoryTest
    extends CliTestCase
{
    @Test
    public void testUsage()
        throws Exception
    {
        run(1, "not-a-command");

        assertThat(stderrText, startsWith("\nUnknown command: 'not-a-command'\n"));
    }
}
