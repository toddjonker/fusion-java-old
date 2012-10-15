// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Namespace.TopBinding;
import java.util.Collection;


final class ModuleBuilderImpl
    implements ModuleBuilder
{
    private final ModuleRegistry  myRegistry;
    private final ModuleNamespace myNamespace;

    ModuleBuilderImpl(ModuleRegistry registry, ModuleIdentity moduleId)
    {
        myRegistry = registry;
        myNamespace = new ModuleNamespace(registry, moduleId);
    }

    @Override
    public void define(String name, Object value)
    {
        myNamespace.bind(name, value);
    }

    ModuleInstance build()
        throws FusionException
    {
        ModuleStore store = new ModuleStore(myNamespace.extractValues());
        Collection<TopBinding> bindings = myNamespace.getBindings();

        // TODO should we register the module?
        return new ModuleInstance(myNamespace.getModuleId(), store, bindings);
    }

    @Override
    public void instantiate()
        throws FusionException
    {
        ModuleInstance module = build();
        myRegistry.register(module);
    }
}
