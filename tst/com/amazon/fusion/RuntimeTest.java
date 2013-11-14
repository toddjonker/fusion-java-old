// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.isTrue;
import static com.amazon.fusion.FusionVoid.isVoid;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonValue;
import java.io.File;
import java.math.BigInteger;
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
//        b.build();

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
        throws Exception
    {
        File file = new File("no-such-file");
        assertFalse(file.exists());
        runtimeBuilder().setInitialCurrentDirectory(file);
    }


    @Test(expected = IllegalArgumentException.class)
    public void testCurrentDirectoryIsNormalFile()
        throws Exception
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
        eval("(require '''/grain''')");
        assertString("soup", "barley");
    }


    @Test
    public void testLoadFile()
        throws Exception
    {
        checkString("hello", loadFile("tst-data/hello.ion"));

        // Test that eval'd define affects the visible namespace
        loadFile("tst-data/trivialDefine.fusion");
        assertEval(3328, "x");

        // Test eval'ing a module
        loadFile("ftst/repo/src/grain.fusion");
    }

    @Test
    public void testNullInjection()
        throws Exception
    {
        Object fv = topLevel().call("identity", (Object) null);
        assertTrue(isVoid(topLevel(), fv));

        // Check that define() injects the given value.
        topLevel().define("v", null);
        fv = topLevel().eval("(identity v)");
        assertTrue(isVoid(topLevel(), fv));
    }

    @Test
    public void testIntInjection()
        throws Exception
    {
        Object fv = topLevel().call("<", 22, BigInteger.valueOf(23));
        assertTrue(isTrue(topLevel(), fv));

        // Inject Byte
        topLevel().define("v", Byte.valueOf(Byte.MAX_VALUE));
        fv = topLevel().eval("(= v " + Byte.MAX_VALUE + ")");
        assertTrue(isTrue(topLevel(), fv));

        // Inject Short
        topLevel().define("v", Short.valueOf(Short.MAX_VALUE));
        fv = topLevel().eval("(= v " + Short.MAX_VALUE + ")");
        assertTrue(isTrue(topLevel(), fv));

        // Inject Integer
        topLevel().define("v", 22);
        fv = topLevel().eval("(= v 22)");
        assertTrue(isTrue(topLevel(), fv));

        // Inject Long
        topLevel().define("v", Long.valueOf(Long.MAX_VALUE));
        fv = topLevel().eval("(= v " + Long.MAX_VALUE + ")");
        assertTrue(isTrue(topLevel(), fv));
    }

    @Test
    public void testBoolInjection()
        throws Exception
    {
        Object fv = topLevel().call("=", true, Boolean.FALSE);
        assertFalse(isTrue(topLevel(), fv));

        topLevel().define("v", true);
        fv = topLevel().eval("v");
        assertTrue(isTrue(topLevel(), fv));
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
        final ModuleIdentity id = ModuleIdentity.internBuiltinName("/tst/dummy");
        assertSame(id, ModuleIdentity.internBuiltinName("/tst/dummy"));

        ModuleBuilder builder = runtime().makeModuleBuilder("/tst/dummy");
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
        builder = runtime().makeModuleBuilder("/tst/dummy");
        try {
            builder.instantiate();
            fail("expected exception");
        }
        catch (ContractException e) { }


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


    @Test
    public void testEvalUsesCurrentIonReaderValue()
        throws Exception
    {
        IonReader r = system().newReader("(define a 338) a");
        Object result = topLevel().eval(r);
        checkLong(338, result);
    }
}
