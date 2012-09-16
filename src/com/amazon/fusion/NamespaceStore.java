// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

interface NamespaceStore
    extends Store
{
    void set(int address, Object value);

    Object lookup(int address);
}
