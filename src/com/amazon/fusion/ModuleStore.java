// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Eval-time storage for modules.
 */
final class ModuleStore
    implements NamespaceStore
{
    private final Object[] myValues;

    ModuleStore(int topLevelVariableCount)
    {
        myValues = new Object[topLevelVariableCount];
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
}
