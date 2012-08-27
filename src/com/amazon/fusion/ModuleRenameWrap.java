// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Syntax wrap that adds all exported bindings from a specific module.
 */
class ModuleRenameWrap
    extends SyntaxWrap
{
    private final ModuleInstance myModule;

    ModuleRenameWrap(ModuleInstance module)
    {
        myModule = module;
    }


    @Override
    Environment.Binding resolve(String ident)
    {
        // Don't go directly to the module's namespace, it exposes things
        // that are not exported!
        return myModule.resolveProvidedBinding(ident);
    }

    @Override
    public String toString()
    {
        return "/* Module renames for " + myModule.identify() + " */";
    }
}
