// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

interface NamespaceStore
    extends Store
{
    ModuleRegistry getRegistry();

    Object lookupImport(int moduleAddress, int bindingAddress);
}
