// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

interface NamespaceStore
    extends Store
{
    @Override
    void set(int address, Object value);

    @Override
    Object lookup(int address);
}
