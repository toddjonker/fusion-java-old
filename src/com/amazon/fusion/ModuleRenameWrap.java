// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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


    @Override
    Binding resolve(String identifier,
                    Iterator<SyntaxWrap> moreWraps,
                    Set<Integer> returnMarks)
    {
        Binding b;
        if (moreWraps.hasNext())
        {
            SyntaxWrap nextWrap = moreWraps.next();
            b = nextWrap.resolve(identifier, moreWraps, returnMarks);
        }
        else
        {
            b = null;
        }

        if (b == null || b instanceof FreeBinding)
        {
            String name = identifier;
            b = myModule.resolveProvidedName(name);
            if (b == null)
            {
                b = new FreeBinding(name);
            }
        }

        return b;
    }


    @Override
    public String toString()
    {
        return "/* Module renames for " + myModule.identify() + " */";
    }
}
