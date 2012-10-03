// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonValue;
import java.io.File;
import org.junit.Test;

public class RuntimeTest
    extends CoreTestCase
{
    @Test
    public void testDefaultCurrentDirectory()
        throws Exception
    {
        assertString(System.getProperty("user.dir"), "(current_directory)");
    }


    @Test(expected = IllegalArgumentException.class)
    public void testCurrentDirectoryDoesNotExist()
    {
        File file = new File("no-such-file");
        assertFalse(file.exists());
        runtimeBuilder().setInitialCurrentDirectory(file);
    }


    @Test(expected = IllegalArgumentException.class)
    public void testCurrentDirectoryIsNormalFile()
    {
        File file = new File("build.xml");
        assertTrue(file.isFile());
        runtimeBuilder().setInitialCurrentDirectory(file);
    }


    @Test
    public void testUseFromCurrentDirectory()
        throws Exception
    {
        File tstRepo = new File("tst-repo");
        runtimeBuilder().setInitialCurrentDirectory(tstRepo);
        eval("(use \"grain\")");
        assertString("soup", "barley");
    }


    @Test
    public void testModuleInUserRepository()
        throws Exception
    {
        useTstRepo();
        eval("(use grain)");
        assertString("soup", "barley");
    }


    @Test
    public void testLoadFile()
        throws Exception
    {
        checkString("hello", loadFile("tst-data/hello.ion"));

        // Test that eval'd define affects the visible namespace
        loadFile("tst-data/trivialDefine.ion");
        assertEval(3328, "x");

        // Test eval'ing a module
        Object mod = loadFile("tst-repo/grain.ion");
        assertTrue(mod instanceof ModuleInstance);
    }


    @Test
    public void testIonize()
        throws Exception
    {
        Object fv = eval("12");
        IonValue iv = runtime().ionizeMaybe(fv, system());
        assertEquals(12, ((IonInt)iv).intValue());
        iv = runtime().ionize(fv, system());
        assertEquals(12, ((IonInt)iv).intValue());

        fv = eval("(lambda () 12)");
        assertSame(null, runtime().ionizeMaybe(fv, system()));
    }

    @Test(expected = FusionException.class)
    public void testBadIonize()
        throws Exception
    {
        Object fv = eval("(lambda () 12)");
        runtime().ionize(fv, system());
    }
}
