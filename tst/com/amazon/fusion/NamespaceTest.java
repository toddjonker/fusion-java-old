// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.KERNEL_MODULE_IDENTITY;
import static com.amazon.fusion.GlobalState.KERNEL_MODULE_NAME;
import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class NamespaceTest
    extends CoreTestCase
{
    @Test
    public void testBaseNamespace()
        throws Exception
    {
        topLevel().define("callback", new Procedure0("callback")
        {
            @Override
            Object doApply(Evaluator eval)
                throws FusionException
            {
                Namespace ns = eval.newBaseNamespace();
                Binding b = ns.resolve("module");
                assertTrue(b.lookup(ns) instanceof ModuleKeyword);

                ModuleRegistry reg = ns.getRegistry();
                ModuleInstance kernel = reg.lookup(KERNEL_MODULE_IDENTITY);
                assertEquals(KERNEL_MODULE_NAME, kernel.getInferredName());

                return null;
            }
        });

        eval("(callback)");
    }
}
