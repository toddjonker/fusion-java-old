// Copyright (c) 2012-2022 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNamespace.makeNamespaceWithLanguage;
import static com.amazon.fusion.GlobalState.KERNEL_MODULE_IDENTITY;
import static com.amazon.fusion.GlobalState.KERNEL_MODULE_NAME;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import com.amazon.ion.IonReader;
import com.amazon.ion.system.IonReaderBuilder;
import org.junit.Before;
import org.junit.Test;

public class NamespaceTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws FusionException
    {
        topLevel().requireModule("/fusion/namespace");
    }


    @Test
    public void testBaseNamespace()
        throws Exception
    {
        topLevel().define("callback", new Procedure1()
        {
            @Override
            Object doApply(Evaluator eval, Object arg)
                throws FusionException
            {
                Namespace ns = (Namespace) arg;
                Object moduleForm = ns.lookup(GlobalState.MODULE);
                assertTrue(moduleForm instanceof ModuleForm);

                ModuleRegistry reg = ns.getRegistry();
                ModuleInstance kernel = reg.lookup(KERNEL_MODULE_IDENTITY);
                assertEquals(KERNEL_MODULE_NAME, kernel.getInferredName());

                return null;
            }
        });

        eval("(callback (make_namespace_with_language \"/fusion\"))");
    }


    @Test
    public void testBadLanguage()
        throws Exception
    {
        expectArgumentExn("(make_namespace_with_language 23)", 0);
        expectArgumentExn("(make_namespace_with_language \"\")", 0);

        // Not absolute module path
        expectArgumentExn("(make_namespace_with_language \"foo\")", 0);
    }



    private void assertLoaded(ModuleRegistry reg, String module)
        throws Exception
    {
        assertTrue(reg.isLoaded(ModuleIdentity.forAbsolutePath(module)));
    }

    private void assertNotLoaded(ModuleRegistry reg, String module)
        throws Exception
    {
        assertFalse(reg.isLoaded(ModuleIdentity.forAbsolutePath(module)));
    }

    private void assertNotAttached(ModuleRegistry reg, String module)
        throws Exception
    {
        assertNull(reg.lookup(ModuleIdentity.forAbsolutePath(module)));
    }

    private void assertAttached(ModuleRegistry reg, String module)
        throws Exception
    {
        assertNotNull(reg.lookup(ModuleIdentity.forAbsolutePath(module)));
    }


    @Test
    public void testRegistrySeparation()
        throws Exception
    {
        StandardRuntime runtime = (StandardRuntime) runtime();

        ModuleRegistry reg0 = runtime.getDefaultRegistry();
        ModuleRegistry reg1 = runtime.makeModuleRegistry();

        assertAttached(reg0, "/fusion/base");
        assertNotLoaded(reg1, "/fusion/base");

        // Since reg1 is empty modulo the kernel, we should get a separate
        // copy of the language when we build a new namespace.
        makeNamespaceWithLanguage(evaluator(), "/fusion/base", reg1);
        assertNotSame(reg0.lookup("/fusion/base"),
                      reg1.lookup("/fusion/base"));
    }


    @Test
    public void testTopLevelLoadSeparation()
        throws Exception
    {
        StandardRuntime runtime = (StandardRuntime) runtime();

        ModuleRegistry reg0 = runtime.getDefaultRegistry();
        ModuleRegistry reg1 = runtime.makeModuleRegistry();

        StandardTopLevel top = runtime.makeTopLevel(reg1, "/fusion");
        IonReader code =
            IonReaderBuilder.standard().build("(module M '/fusion' true)");
        top.loadModule("/fresh", code, null);
        assertNotLoaded(reg0, "/fresh");
        assertLoaded(reg1, "/fresh");
        assertNotAttached(reg1, "/fresh"); // loadModule() doesn't instantiate.
        top.requireModule("/fresh");
        assertAttached(reg1, "/fresh");
        assertNotLoaded(reg0, "/fresh");
    }
}
