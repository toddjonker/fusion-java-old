// Copyright (c) 2019-2024 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static org.hamcrest.Matchers.containsString;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import java.io.File;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TemporaryFolder;

public class DocumentTest
    extends CliTestCase
{
    @Rule
    public TemporaryFolder tmpDir = new TemporaryFolder();

    private File noSuchFile()
        throws Exception
    {
        File f = new File(tmpDir.getRoot(), "no-such-file");
        assertFalse(f.exists());
        return f;
    }

    private File plainFile()
        throws Exception
    {
        File f = tmpDir.newFile();
        assert(f.isFile());
        return f;
    }

    private File outputDir()
        throws Exception
    {
        return tmpDir.newFolder("docs-test-output");
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
        String repo = tmpDir.newFolder().getPath();

        run(1, "document", outputDir().getPath(), repo);

        assertThat(stderrText, containsString("has no src"));
        assertThat(stderrText, containsString(repo));
    }
}
