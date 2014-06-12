// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.KERNEL_MODULE_IDENTITY;
import static com.amazon.fusion.GlobalState.KERNEL_MODULE_NAME;
import static org.junit.Assert.assertEquals;
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
}
