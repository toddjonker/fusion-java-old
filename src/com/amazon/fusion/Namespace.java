// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 *
 */
class Namespace
    implements Environment
{
    class NsBinding implements Binding
    {
        FusionValue myValue;
    }

    private final ModuleRegistry myRegistry;
    private final Map<String,NsBinding> myBindings =
        new HashMap<String,NsBinding>();

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
        NsBinding binding = myBindings.get(name);
        if (binding == null)
        {
            binding = new NsBinding();
            binding.myValue = value;
            myBindings.put(name, binding);
        }
        else
        {
            binding.myValue = value;
        }
    }

    void use(ModuleIdentity id)
    {
        ModuleInstance module = myRegistry.lookup(id);
        use(module);
    }

    void use(ModuleInstance module)
    {
        module.visitProvidedBindings(new BindingVisitor()
        {
            @Override
            public void visitBinding(String name, NsBinding binding)
            {
                myBindings.put(name, binding);
            }
        });
    }


    @Override
    public NsBinding resolve(String name)
    {
        return myBindings.get(name);
    }


    @Override
    public FusionValue lookup(String name)
    {
        NsBinding binding = myBindings.get(name);
        if (binding != null) return binding.myValue;
        return null;
    }

    @Override
    public void collectNames(Collection<String> names)
    {
        names.addAll(myBindings.keySet());
    }


    void visitAllBindings(BindingVisitor v)
    {
        for (Map.Entry<String, NsBinding> entry : myBindings.entrySet())
        {
            v.visitBinding(entry.getKey(), entry.getValue());
        }
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
