// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static junit.framework.Assert.assertTrue;
import static org.junit.Assert.assertSame;
import org.junit.Test;

/**
 *
 */
public class NamespaceTest
    extends CoreTestCase
{
    @Test
    public void testBaseNamespace()
        throws Exception
    {
        runtime().bind("callback", new Procedure("callback")
        {
            @Override
            FusionValue invoke(Evaluator eval, FusionValue[] args)
                throws FusionException
            {
                Namespace ns = eval.newBaseNamespace();
                assertTrue(ns.lookup("module") instanceof ModuleKeyword);

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
        runtime().bind("callback", new Procedure("callback")
        {
            @Override
            FusionValue invoke(Evaluator eval, FusionValue[] args)
                throws FusionException
            {
                ModuleRegistry registry = eval.getModuleRegistry();
                Namespace ns = eval.newBaseNamespace();


                ModuleIdentity id = ModuleIdentity.intern("dummy");
                assertSame(id, ModuleIdentity.intern("dummy"));

                ModuleInstance mod = new ModuleInstance(id, ns);
                registry.register(mod);
                assertSame(mod, registry.lookup(id));

                // TODO test registering two instances w/ same identity
                Namespace ns2 = new Namespace(new ModuleRegistry());
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
