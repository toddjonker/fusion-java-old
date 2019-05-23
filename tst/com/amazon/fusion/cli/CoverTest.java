// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.isEmptyString;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class CoverTest
    extends CliTestCase
{
    @Rule
    public final TemporaryFolder myFolder = new TemporaryFolder();


    private File plainFile()
    {
        File f = new File("Config");
        assert(f.isFile());
        return f;
    }

    private File dataDir()
        throws IOException
    {
        return myFolder.newFolder("data");
    }

    private File reportDir()
        throws IOException
    {
        return myFolder.newFolder("report");
    }


    @Test
    public void testNoArgs()
        throws Exception
    {
        run(1, "report_coverage");
        assertThat(stderrText, containsString("Usage:"));
    }


    @Test
    public void testDataDirIsMissing()
        throws Exception
    {
        File f = new File(myFolder.getRoot(), "no file");
        assertFalse(f.exists());

        run(1, "report_coverage", f.getPath(), reportDir().getPath());

        assertThat(stderrText, containsString("not a directory"));
        assertThat(stderrText, containsString(f.getPath()));
    }

    @Test
    public void testDataDirIsFile()
        throws Exception
    {
        String f = plainFile().getPath();

        run(1, "report_coverage", f, reportDir().getPath());

        assertThat(stderrText, containsString("not a directory"));
        assertThat(stderrText, containsString(f));
    }

    @Test
    public void testReportDirIsFile()
        throws Exception
    {
        String f = plainFile().getPath();

        run(1, "report_coverage", dataDir().getPath(), f);

        assertThat(stderrText, containsString("not a directory"));
        assertThat(stderrText, containsString(f));
    }


    @Test
    public void testCoverCompletionMessage()
        throws Exception
    {
        String dataDir   = dataDir().getPath();
        String reportDir = reportDir().getPath();

        // I'm surprised this works without any coverage data!
        run(0, "report_coverage", dataDir, reportDir);
        assertThat(stdoutText,
                   containsString("Wrote Fusion coverage report to " + reportDir));
        assertThat(stderrText, isEmptyString());

        assertTrue(new File(reportDir, "index.html").isFile());
    }
}
