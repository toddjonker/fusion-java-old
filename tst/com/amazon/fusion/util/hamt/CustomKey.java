// Copyright (c) 2018-2021 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.util.hamt;

/**
 * This class allows us to easily form hash collisions for different keys.
 */
class CustomKey
{
    final int hash;
    final Object key;

    CustomKey(int hash, Object key)
    {
        this.hash = hash;
        this.key = key;
    }

    @Override
    public String toString()
    {
        return "Hash: " + hash + " Key: " + key;
    }

    @Override
    public int hashCode()
    {
        return hash;
    }

    @Override
    public boolean equals(Object obj)
    {
        if (obj instanceof CustomKey)
        {
            return key.equals(((CustomKey) obj).key);
        }
        return false;
    }
}
