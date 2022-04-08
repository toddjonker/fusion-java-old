// Copyright (c) 2012-2022 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.KERNEL_MODULE_IDENTITY;
import static com.amazon.fusion.GlobalState.KERNEL_MODULE_NAME;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;

public class NamespaceTest
    extends CoreTestCase
{
    @Before
    public void requires()
        throws FusionException
    {
        useTstRepo();
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


    private void assertLoaded(StandardTopLevel top, String module)
        throws Exception
    {
        assertTrue(top.getRegistry().isLoaded(module));
    }

    private void assertNotLoaded(StandardTopLevel top, String module)
        throws Exception
    {
        assertFalse(top.getRegistry().isLoaded(module));
    }

    private void assertNotAttached(StandardTopLevel top, String module)
        throws Exception
    {
        assertNull(top.getRegistry().lookup(module));
    }

    private void assertAttached(StandardTopLevel top, String module)
        throws Exception
    {
        assertNotNull(top.getRegistry().lookup(module));
    }


    private void assertSameInstances(StandardTopLevel top0,
                                     StandardTopLevel top1,
                                     String module)
    {
        ModuleInstance inst0 = top0.getRegistry().lookup(module);
        ModuleInstance inst1 = top1.getRegistry().lookup(module);
        assertNotNull(inst0);
        assertSame(inst0, inst1);
    }

    private void assertDifferentInstances(StandardTopLevel top0,
                                          StandardTopLevel top1,
                                          String module)
    {
        ModuleInstance inst0 = top0.getRegistry().lookup(module);
        ModuleInstance inst1 = top1.getRegistry().lookup(module);
        assertNotNull(inst0);
        assertNotNull(inst1);
        assertNotSame(inst0, inst1);
    }



    /**
     * New registries must contain any essential low-level modules that must not
     * be duplicated.
     */
    @Test
    public void testNewRegistryHasKernel()
        throws Exception
    {
        StandardRuntime runtime = (StandardRuntime) runtime();

        StandardTopLevel top0 = runtime.getDefaultTopLevel();
        StandardTopLevel top1 = runtime.makeEmptyTopLevelAndRegistry();

        assertSameInstances(top0, top1, KERNEL_MODULE_NAME);
        assertNotLoaded(top1, "/fusion");
        assertNotLoaded(top1, "/fusion/base");
    }


    @Test
    public void testRegistrySeparation()
        throws Exception
    {
        StandardRuntime runtime = (StandardRuntime) runtime();

        StandardTopLevel top0 = runtime.getDefaultTopLevel();
        StandardTopLevel top1 = runtime.makeEmptyTopLevelAndRegistry();

        assertAttached(top0, "/fusion/base");
        assertNotLoaded(top1, "/fusion/base");

        // Since top1's registry is empty modulo the kernel, we should get a
        // separate copy of any module we require.
        top1.requireModule("/fusion/base");
        assertDifferentInstances(top0, top1, "/fusion/base");
    }


    @Test
    public void testTopLevelLoadSeparation()
        throws Exception
    {
        StandardRuntime runtime = (StandardRuntime) runtime();

        StandardTopLevel top0 = runtime.getDefaultTopLevel();
        StandardTopLevel top1 = runtime.makeEmptyTopLevelAndRegistry();

        top1.loadModule("/grain");
        assertNotLoaded(top0, "/grain");
        assertLoaded(top1, "/grain");
        assertNotAttached(top1, "/grain"); // loadModule() doesn't instantiate.

        top1.requireModule("/grain");
        assertAttached(top1, "/grain");
        assertNotLoaded(top0, "/grain");
    }
}
