// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertSame;
import org.junit.Test;

public class NamespaceTest
    extends CoreTestCase
{
    @Test
    public void testBaseNamespace()
        throws Exception
    {
        topLevel().define("callback", new Procedure("callback")
        {
            @Override
            Object doApply(Evaluator eval, Object[] args)
                throws FusionException
            {
                Namespace ns = eval.newBaseNamespace();
                Binding b = ns.resolve("module");
                assertTrue(b.lookup(ns) instanceof ModuleKeyword);

                ModuleRegistry reg = ns.getRegistry();
                ModuleInstance kernel = reg.lookup(KernelModule.IDENTITY);
                assertTrue(kernel instanceof KernelModule);

                return null;
            }
        });

        eval("(callback)");
    }


    @Test
    public void testBasicLookup()
        throws Exception
    {
        topLevel().define("callback", new Procedure("callback")
        {
            @Override
            Object doApply(Evaluator eval, Object[] args)
                throws FusionException
            {
                ModuleRegistry registry = eval.getModuleRegistry();

                ModuleIdentity id = ModuleIdentity.intern("dummy");
                assertSame(id, ModuleIdentity.intern("dummy"));

                ModuleNamespace ns = new ModuleNamespace(registry, id);
                ModuleInstance mod = new ModuleInstance(id, ns);
                registry.register(mod);
                assertSame(mod, registry.lookup(id));

                // Test registering two instances w/ same identity
                ModuleNamespace ns2 = new ModuleNamespace(new ModuleRegistry(),
                                                          id);
                ModuleInstance mod2 = new ModuleInstance(id, ns2);

                try {
                    registry.register(mod2);
                }
                catch (ContractFailure e) { }

                return null;
            }
        });

        eval("(callback)");
    }

}
