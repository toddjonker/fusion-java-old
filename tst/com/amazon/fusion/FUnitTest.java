// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.junit.Injected;
import com.amazon.fusion.junit.Injected.Inject;
import com.amazon.ion.IonReader;
import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.Test;
import org.junit.runner.RunWith;

/**
 * Evaluates all the files under the 'ftst' source directory.
 * If evaluation completes without failure, its assumed to be a passing test.
 */
@RunWith(Injected.class)
public class FUnitTest
    extends CoreTestCase
{
    @Inject("testFile")
    public static final File[] FILES =
        gatherFiles(/* recurse */ true, "ftst");

    private File myTestFile;

    public void setTestFile(File file)
    {
        myTestFile = file;
    }


    @Test
    public void run()
        throws Exception
    {
        useTstRepo();

        FileInputStream source = new FileInputStream(myTestFile);
        try
        {
            SourceName name = SourceName.forFile(myTestFile);
            IonReader reader = system().newReader(source);
            runtime().eval(reader, name);
        }
        finally
        {
            source.close();
        }
        // yay
    }


    //========================================================================

    // Helper methods forked from com.amazon.ion.TestUtils

    private static void gatherFiles(List<File> results,
                                    boolean recurse,
                                    File dir)
    {
        String[] fileNames = dir.list();
        if (fileNames == null)
        {
            String message = "Not a directory: " + dir.getAbsolutePath();
            throw new IllegalArgumentException(message);
        }

        // Sort the fileNames so they are listed in order.
        // This is not a functional requirement but it helps humans scanning
        // the output looking for a specific file.
        Arrays.sort(fileNames);

        for (String fileName : fileNames)
        {
            File testFile = new File(dir, fileName);
            if (testFile.isDirectory())
            {
                if (recurse)
                {
                    gatherFiles(results, recurse, testFile);
                }
            }
            else
            {
                results.add(testFile);
            }
        }
    }

    private static File[] gatherFiles(boolean recurse,
                                      String... directories)
    {
        ArrayList<File> files = new ArrayList<File>();

        for (String dirName : directories)
        {
            File dir = new File(dirName);
            if (! dir.isDirectory())
            {
                String message =
                    "not a directory: "
                        + dir.getAbsolutePath();
                throw new IllegalArgumentException(message);
            }

            gatherFiles(files, recurse, dir);
        }

        return files.toArray(new File[files.size()]);
    }
}
