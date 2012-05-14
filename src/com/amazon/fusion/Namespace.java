// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 */
class Namespace
    implements Environment
{
    private final ModuleRegistry myRegistry;
    private final Map<String,FusionValue> myBindings =
        new HashMap<String,FusionValue>();

    Namespace(ModuleRegistry registry)
    {
        myRegistry = registry;
    }


    public ModuleRegistry getRegistry()
    {
        return myRegistry;
    }

    @Override
    public Namespace namespace()
    {
        return this;
    }


    //========================================================================


    public void bind(String name, FusionValue value)
    {
        value.inferName(name);
        myBindings.put(name, value);
    }

    void use(ModuleIdentity id)
    {
        ModuleInstance module = myRegistry.lookup(id);
        use(module);
    }

    void use(ModuleInstance module)
    {
        Namespace moduleNamespace = module.getNamespace();
        Collection<String> names = new ArrayList<String>();
        moduleNamespace.collectNames(names);
        for (String name : names)
        {
            FusionValue value = moduleNamespace.lookup(name);
            myBindings.put(name, value);
        }
    }

    @Override
    public FusionValue lookup(String name)
    {
        return myBindings.get(name);
    }

    @Override
    public void collectNames(Collection<String> names)
    {
        names.addAll(myBindings.keySet());
    }


    //========================================================================


    /**
     * Creates a new namespace sharing the same {@link ModuleRegistry}.
     */
    Namespace emptyNamespace()
    {
        return new Namespace(myRegistry);
    }
}
