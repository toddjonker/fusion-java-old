// Copyright (c) 2012-2024 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.junit;

import static com.amazon.fusion.CoreTestCase.ftstRepositoryDirectory;
import static com.amazon.fusion.CoreTestCase.ftstScriptDirectory;
import static com.amazon.fusion.CoreTestCase.fusionBootstrapDirectory;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Rule;
import org.junit.runner.Description;
import org.junit.runner.notification.RunNotifier;
import org.junit.runners.ParentRunner;
import org.junit.runners.model.Statement;

/**
 * Evaluates all the files under the 'ftst' source directory.
 * If evaluation completes without failure, its assumed to be a passing test.
 * <p>
 * Classes using this runner can also use {@link BeforeClass} and
 * {@link AfterClass} annotations but not {@link Before} or {@link After}.
 * Similarly, {@link ClassRule}s are supported but not {@link Rule}s.
 */
public class SimpleScriptRunner
    extends ParentRunner<File>
{
    /**
     * Test action that runs a single Fusion script.
     */
    private static final class FusionScriptStatement
        extends Statement
    {
        private final SimpleScriptRunner myRunner;
        private final File myScript;

        private FusionScriptStatement(SimpleScriptRunner runner, File script)
        {
            myRunner = runner;
            myScript = script;
        }

        @Override
        public void evaluate()
            throws Throwable
        {
            myRunner.load(myScript);
        }
    }


    //========================================================================


    private final FusionRuntime myRuntime;


    public SimpleScriptRunner(Class<?> testClass)
        throws Throwable
    {
        super(testClass);

        FusionRuntimeBuilder b = FusionRuntimeBuilder.standard();

        // This allows tests to run in an IDE, so that we don't have to copy the
        // bootstrap repo into the classpath.  In scripted builds, this has no
        // effect since the classpath includes the code, which will shadow the
        // content of this directory.
        b = b.withBootstrapRepository(fusionBootstrapDirectory().toFile());

        // Enable this to have coverage collected during an IDE run.
//      b = b.withCoverageDataDirectory(new File("build/private/fcoverage"));

        // This has no effect in an IDE, since this file is not on its copy of
        // the test classpath.  In scripted builds, this provides the coverage
        // configuration. Historically, it also provided the bootstrap repo.
        b = b.withConfigProperties(testClass, "/fusion.properties");

        b.addRepositoryDirectory(ftstRepositoryDirectory().toFile());

        myRuntime = b.build();
    }

    void load(File script)
        throws FusionException
    {
        myRuntime.makeTopLevel().load(script);
    }

    @Override
    protected List<File> getChildren()
    {
        ArrayList<File> files = new ArrayList<>();
        gatherFiles(files, ftstScriptDirectory().toFile());
        return files;
    }

    @Override
    protected Description describeChild(File child)
    {
        Class<?> klass = getTestClass().getJavaClass();
        String name = child.getPath();
        return Description.createTestDescription(klass, name);
    }

    @Override
    protected void runChild(File child, RunNotifier notifier)
    {
        Statement s = new FusionScriptStatement(this, child);
        Description d = describeChild(child);
        runLeaf(s,d, notifier);
    }


    //========================================================================


    // Forked from com.amazon.ion.TestUtils
    private static void gatherFiles(List<File> results,
                                    File dir)
    {
        String[] fileNames = dir.list();

        // Sort the fileNames so they are listed in order.
        // This is not a functional requirement but it helps humans scanning
        // the output looking for a specific file.
        Arrays.sort(fileNames);

        for (String fileName : fileNames)
        {
            File testFile = new File(dir, fileName);
            if (testFile.isDirectory())
            {
                gatherFiles(results, testFile);
            }
            else if (fileName.endsWith(".test.fusion"))
            {
                results.add(testFile);
            }
        }
    }
}
