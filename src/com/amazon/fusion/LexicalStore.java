// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Eval-time storage for a lexical scope.
 */
final class LexicalStore
    implements Store
{
    private final Store    myEnclosure;
    private final Object[] myValues;

    /**
     * @param values CALLEE ASSUMES OWNERSHIP OF THIS ARRAY
     */
    LexicalStore(Store enclosure, Object[] values)
    {
        myEnclosure = enclosure;
        myValues    = values;
    }

    @Override
    public NamespaceStore namespace()
    {
        // TODO FUSION-53 link directly to namespace
        return myEnclosure.namespace();
    }

    @Override
    public Object lookup(int rib, int address)
    {
        if (rib == 0) return myValues[address];
        return myEnclosure.lookup(rib - 1, address);
    }
}
