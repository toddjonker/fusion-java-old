// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import java.util.Iterator;
import java.util.Set;

/**
 * Syntax wrap that adds all exported bindings from a specific module.
 */
class ModuleRenameWrap
    extends SyntaxWrap
{
    private final ModuleInstance myModule;

    ModuleRenameWrap(ModuleInstance module)
    {
        assert module != null;
        myModule = module;
    }

    ModuleBinding localResolveMaybe(String name)
    {
        return myModule.resolveProvidedName(name);
    }

    @Override
    Binding resolve(String name,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<Integer> returnMarks)
    {
        Binding local = myModule.resolveProvidedName(name);
        if (local != null) return local;

        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            return nextWrap.resolve(name, moreWraps, returnMarks);
        }

        return null;
    }


    @Override
    Iterator<SyntaxWrap> iterator()
    {
        return null;
    }


    @Override
    public String toString()
    {
        String id = myModule.getIdentity().internString();
        return "{{{Module renames for " + id + "}}}";
    }
}
