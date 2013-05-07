// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.isVoid;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonValue;
import java.io.File;
import org.junit.Test;

public class RuntimeTest
    extends CoreTestCase
{
    private static final File TEST_REPO = new File("tst-repo");


    @Test
    public void testDefaultBuilder()
        throws Exception
    {
        FusionRuntimeBuilder b = FusionRuntimeBuilder.standard();
        assertSame(b, b.mutable());
        b.build();
    }

    @Test
    public void testDefaultCurrentDirectory()
        throws Exception
    {
        topLevel().requireModule("/fusion/io");
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

    @Test(expected = UnsupportedOperationException.class)
    public void testInitialCurrentDirectoryImmutability()
        throws Exception
    {
        runtimeBuilder().immutable().setInitialCurrentDirectory(TEST_REPO);
    }

    @Test
    public void testUseFromCurrentDirectory()
        throws Exception
    {
        runtimeBuilder().setInitialCurrentDirectory(TEST_REPO);
        eval("(use \"grain\")");
        assertString("soup", "barley");
    }


    @Test(expected = UnsupportedOperationException.class)
    public void testDocumentingImmutability()
        throws Exception
    {
        runtimeBuilder().immutable().setDocumenting(true);
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
    public void testNullInjection()
        throws Exception
    {
        topLevel().requireModule("/fusion/function");
        Object fv = topLevel().call("identity", (Object) null);
        assertTrue(isVoid(topLevel(), fv));
    }

    @Test
    public void testVoidReturn()
        throws Exception
    {
        Object fv = eval("(void)");
        assertTrue(isVoid(topLevel(), fv));
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
    public void testBadIonizeProcedure()
        throws Exception
    {
        Object fv = eval("(lambda () 12)");
        runtime().ionize(fv, system());
    }

    @Test(expected = FusionException.class)
    public void testBadIonizeVoid()
        throws Exception
    {
        Object fv = eval("(void)");
        runtime().ionize(fv, system());
    }


    @Test
    public void testModuleRegistration()
        throws Exception
    {
        final ModuleIdentity id = ModuleIdentity.internBuiltinName("#%dummy");
        assertSame(id, ModuleIdentity.internBuiltinName("#%dummy"));

        ModuleBuilder builder = runtime().makeModuleBuilder("#%dummy");
        builder.instantiate();

        topLevel().define("callback", new Procedure0("callback")
        {
            @Override
            Object doApply(Evaluator eval)
                throws FusionException
            {
                ModuleRegistry registry =
                    eval.findCurrentNamespace().getRegistry();
                ModuleInstance mod = registry.lookup(id);
                assertNotNull(mod);

                return null;
            }
        });

        eval("(callback)");


        // Test registering two instances w/ same identity
        builder = runtime().makeModuleBuilder("#%dummy");
        try {
            builder.instantiate();
            fail("expected exception");
        }
        catch (ContractFailure e) { }


        try {
            runtime().makeModuleBuilder("dummy");
            fail("expected exception");
        }
        catch (IllegalArgumentException e) { }

        try {
            runtime().makeModuleBuilder("dum/my");
            fail("expected exception");
        }
        catch (IllegalArgumentException e) { }
    }

    @Test
    public void testTopLevel()
        throws Exception
    {
        useTstRepo();
        runtime().makeTopLevel("/let");
    }
}
