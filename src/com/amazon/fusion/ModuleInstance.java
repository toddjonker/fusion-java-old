// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 *
 */
class ModuleInstance
    extends NamedValue
{
    private final Namespace myNamespace;

    ModuleInstance(Namespace namespace)
    {
        myNamespace = namespace;
    }


    Namespace getNamespace()
    {
        return myNamespace;
    }


    @Override
    final void identify(Appendable out)
        throws IOException
    {
        String name = getInferredName();
        if (name == null)
        {
            out.append("anonymous module");
        }
        else
        {
            out.append("module ");
            IonTextUtils.printQuotedSymbol(out, name);
        }
    }
}
