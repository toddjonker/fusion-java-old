// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionSymbol.BaseSymbol;

interface NamespaceStore
    extends Store
{
    ModuleRegistry getRegistry();

    ModuleStore lookupRequiredModule(int moduleAddress);

    Object lookupImport(int moduleAddress, int bindingAddress);

    /**
     * Currently only implemented for modules.
     */
    BaseSymbol getDefinedName(int address);
}
