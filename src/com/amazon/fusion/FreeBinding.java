// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Environment.Binding;

final class FreeBinding implements Binding
{
    private final String myName;

    FreeBinding(String name)
    {
        myName = name;
    }

    String getName()
    {
        return myName;
    }


    @Override
    public Binding originalBinding()
    {
        return this;
    }


    @Override
    public FusionValue lookup(Environment store)
    {
        return null;
    }


    @Override
    public boolean equals(Object other)
    {
        if (this == other) return true;
        if (! (other instanceof FreeBinding)) return false;

        FreeBinding that = (FreeBinding) other;

        // TODO intern symbol names and use ==
        return this.myName.equals(that.myName);
    }


    @Override
    public String toString()
    {
        return "{{FreeBinding " + myName + "}}";
    }
}
