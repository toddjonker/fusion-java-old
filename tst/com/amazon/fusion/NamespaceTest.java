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
    public void testBaseNamespace() // TODO move to EvaluatorTest
    {
        Evaluator eval = new Evaluator(system());

        Namespace ns = eval.newBaseNamespace();
        assertTrue(ns.lookup("module") instanceof ModuleKeyword);

        ModuleRegistry reg = ns.getRegistry();
        ModuleInstance base = reg.lookup(BaseModule.BASE_IDENTITY);
        assertTrue(base instanceof BaseModule);
    }


    @Test
    public void testBasicLookup()
        throws Exception
    {
        ModuleRegistry registry = new ModuleRegistry();
        Namespace ns = new Namespace(registry);

        ModuleIdentity id = ModuleIdentity.intern("dummy");
        assertSame(id, ModuleIdentity.intern("dummy"));

        ModuleInstance mod = new ModuleInstance(id, ns);
        assertSame(mod, registry.lookup(id));

        // TODO test registering two instances w/ same identity
        Namespace ns2 = new Namespace(new ModuleRegistry());
        ModuleInstance mod2 = new ModuleInstance(id, ns2);

        try {
            registry.register(mod2);
        }
        catch (ContractFailure e) { }
    }

}
