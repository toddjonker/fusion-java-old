// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import java.io.File;
import org.junit.Test;

public class DocumentTest
    extends CliTestCase
{
    private File noSuchFile()
    {
        File f = new File("no-such-file");
        assertFalse(f.exists());
        return f;
    }

    private File plainFile()
    {
        File f = new File("Config");
        assert(f.isFile());
        return f;
    }

    private File outputDir()
    {
        return new File("build/private/docs-test-output");
    }

    private File nonRepoDir()
    {
        File d = new File("build/private");
        assert(d.isDirectory());
        return d;
    }


    @Test
    public void testOutputDirIsFile()
        throws Exception
    {
        String aFile = plainFile().getPath();

        run(1, "document", aFile, "fusion");

        assertThat(stderrText, containsString(aFile));
    }

    @Test
    public void testRepoIsMissing()
        throws Exception
    {
        String repo = noSuchFile().getPath();

        run(1, "document", outputDir().getPath(), repo);

        assertThat(stderrText, containsString("not a directory"));
        assertThat(stderrText, containsString(repo));
    }

    @Test
    public void testRepoIsFile()
        throws Exception
    {
        String repo = plainFile().getPath();

        run(1, "document", outputDir().getPath(), repo);

        assertThat(stderrText, containsString("not a directory"));
        assertThat(stderrText, containsString(repo));
    }

    @Test
    public void testRepoHasNoSrc()
        throws Exception
    {
        String repo = nonRepoDir().getPath();

        run(1, "document", outputDir().getPath(), repo);

        assertThat(stderrText, containsString("has no src"));
        assertThat(stderrText, containsString(repo));
    }
}
