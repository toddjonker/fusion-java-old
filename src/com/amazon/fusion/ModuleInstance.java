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
    private final String[] myProvidedNames;

    /**
     * Creates a module that {@code provide}s all names in its namespace.
     *
     * @throws ContractFailure if the namespace's registry already has a
     * module with the given identity.
     */
    ModuleInstance(ModuleIdentity identity, Namespace namespace)
        throws FusionException, ContractFailure
    {
        this(identity, namespace, null);
    }

    /**
     * Creates a module that {@code provide}s only the given names.
     *
     * @throws ContractFailure if the namespace's registry already has a
     * module with the given identity.
     */
    ModuleInstance(ModuleIdentity identity, Namespace namespace,
                   String[] providedNames)
        throws FusionException, ContractFailure
    {
        myIdentity = identity;
        myNamespace = namespace;
        myProvidedNames = providedNames;

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


    void visitProvidedBindings(BindingVisitor v)
    {
        if (myProvidedNames == null)
        {
            myNamespace.visitAllBindings(v);
        }
        else
        {
            for (String name : myProvidedNames)
            {
                Namespace.NsBinding b = myNamespace.resolve(name);
                v.visitBinding(name, b);
            }
        }
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
