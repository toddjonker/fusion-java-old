// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;

/**
 * A module that's been instantiated for use by one or more other modules.
 * A module has a unique {@link ModuleIdentity} and a {@link Namespace}
 * holding its top-level bindings.
 */
class ModuleInstance
    extends NamedValue
{
    private final ModuleIdentity myIdentity;
    private final Namespace myNamespace;

    /**
     * @throws ContractFailure if the namespace's registry already has a
     * module with the given identity.
     */
    ModuleInstance(ModuleIdentity identity, Namespace namespace)
        throws FusionException, ContractFailure
    {
        myIdentity = identity;
        myNamespace = namespace;

        namespace.getRegistry().register(this);
        inferName(identity.toString());
    }


    ModuleIdentity getIdentity()
    {
        return myIdentity;
    }


    Namespace getNamespace()
    {
        return myNamespace;
    }


    //========================================================================


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
