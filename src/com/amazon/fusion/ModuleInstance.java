// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ModuleNamespace.ModuleBinding;
import com.amazon.fusion.Namespace.TopBinding;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A module that's been instantiated for use by one or more other modules.
 * A module has a unique {@link ModuleIdentity} and a {@link NamespaceStore}
 * holding its top-level bindings.
 */
final class ModuleInstance
    extends NamedValue
{
    private final ModuleIdentity myIdentity;
    private final NamespaceStore myNamespace;

    /**
     * Not all of these bindings are for this module; names that are imported
     * and exported have their bindings passed through.
     */
    private final Map<String,ModuleBinding> myProvidedBindings;


    private ModuleInstance(ModuleIdentity identity, NamespaceStore namespace,
                           int bindingCount)
        throws FusionException, ContractFailure
    {
        myIdentity = identity;
        myNamespace = namespace;
        myProvidedBindings = new HashMap<String,ModuleBinding>(bindingCount);

        inferName(identity.toString());
    }

    /**
     * Creates a module that {@code provide}s the given bindings.
     */
    ModuleInstance(ModuleIdentity identity, NamespaceStore namespace,
                   Collection<TopBinding> bindings)
        throws FusionException, ContractFailure
    {
        this(identity, namespace, bindings.size());

        for (TopBinding binding : bindings)
        {
            String name = binding.getName();

            myProvidedBindings.put(name, (ModuleBinding) binding);
        }
    }

    /**
     * Creates a module that {@code provide}s the given bound identifiers.
     */
    ModuleInstance(ModuleIdentity identity, NamespaceStore namespace,
                   SyntaxSymbol[] providedIdentifiers)
        throws FusionException, ContractFailure
    {
        this(identity, namespace, providedIdentifiers.length);

        for (SyntaxSymbol identifier : providedIdentifiers)
        {
            String  name    = identifier.stringValue();
            Binding binding = identifier.resolve();

            myProvidedBindings.put(name, (ModuleBinding) binding);
        }
    }


    ModuleIdentity getIdentity()
    {
        return myIdentity;
    }


    NamespaceStore getNamespace()
    {
        return myNamespace;
    }


    //========================================================================

    Set<String> providedNames()
    {
        return Collections.unmodifiableSet(myProvidedBindings.keySet());
    }


    /**
     * @return null if the name isn't provided by this module.
     */
    ModuleBinding resolveProvidedName(String name)
    {
        return myProvidedBindings.get(name);
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
