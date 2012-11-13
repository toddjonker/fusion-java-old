// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Eval-time storage for modules.
 */
final class ModuleStore
    implements NamespaceStore
{
    private final ModuleRegistry myRegistry;
    private final ModuleStore[]  myRequiredModules;
    private final Object[]       myValues;


    ModuleStore(ModuleRegistry registry,
                ModuleStore[] requiredModules,
                int topLevelVariableCount)
    {
        myRegistry = registry;
        myRequiredModules = requiredModules;
        myValues = new Object[topLevelVariableCount];
    }

    ModuleStore(ModuleRegistry registry, Object[] values)
    {
        myRegistry = registry;
        myRequiredModules = new ModuleStore[0];
        myValues = values;
    }

    @Override
    public Object lookup(int rib, int address)
    {
        throw new IllegalStateException("Rib not found");
    }

    @Override
    public void set(int rib, int address, Object value)
    {
        throw new IllegalStateException("Rib not found");
    }


    @Override
    public ModuleRegistry getRegistry()
    {
        return myRegistry;
    }

    @Override
    public NamespaceStore namespace()
    {
        return this;
    }

    @Override
    public void set(int address, Object value)
    {
        myValues[address] = value;
    }

    @Override
    public Object lookup(int address)
    {
        return myValues[address];
    }

    @Override
    public Object lookupImport(int moduleAddress, int bindingAddress)
    {
        return myRequiredModules[moduleAddress].myValues[bindingAddress];
    }
}
