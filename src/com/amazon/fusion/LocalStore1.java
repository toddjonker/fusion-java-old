// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Eval-time storage for a lexical scope with exactly one entry.
 */
final class LocalStore1
    implements Store
{
    private final Store          myEnclosure;
    private final NamespaceStore myNamespace;
    private       Object         myValue;

    /**
     * @param value the (initial) bound value.
     */
    LocalStore1(Store enclosure, Object value)
    {
        myEnclosure = enclosure;
        myNamespace = enclosure.namespace();
        myValue     = value;
    }

    @Override
    public NamespaceStore namespace()
    {
        return myNamespace;
    }

    @Override
    public Object lookup(int address)
    {
        return myValue;
    }

    @Override
    public Object lookup(int rib, int address)
    {
        if (rib == 0)
        {
            return myValue;
        }
        return myEnclosure.lookup(rib - 1, address);
    }

    @Override
    public void set(int address, Object value)
    {
        myValue = value;
    }

    @Override
    public void set(int rib, int address, Object value)
    {
        if (rib == 0)
        {
            myValue = value;
        }
        else
        {
            myEnclosure.set(rib - 1, address, value);
        }
    }
}
