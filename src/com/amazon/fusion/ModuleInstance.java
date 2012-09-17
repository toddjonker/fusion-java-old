// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Namespace.TopBinding;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * A module that's been instantiated for use by one or more other modules.
 * A module has a unique {@link ModuleIdentity} and a {@link NamespaceStore}
 * holding its top-level bindings.
 */
class ModuleInstance
    extends NamedValue
{
    private final ModuleIdentity myIdentity;
    private final NamespaceStore myNamespace;

    /**
     * Not all of these bindings are for this module; names that are imported
     * and exported have their bindings passed through.
     */
    private final Map<String,Binding> myProvidedBindings;


    /**
     * Creates a module that {@code provide}s all names in its namespace.
     *
     * @throws ContractFailure if the namespace's registry already has a
     * module with the given identity.
     */
    ModuleInstance(ModuleIdentity identity, ModuleNamespace namespace)
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
    ModuleInstance(ModuleIdentity identity, NamespaceStore namespace,
                   SyntaxSymbol[] providedIdentifiers)
        throws FusionException, ContractFailure
    {
//        assert identity == namespace.getModuleId();

        myIdentity = identity;
        myNamespace = namespace;

        if (providedIdentifiers == null)
        {
            myProvidedBindings = new HashMap<String,Binding>();
        }
        else
        {
            myProvidedBindings =
                new HashMap<String,Binding>(providedIdentifiers.length);

            for (SyntaxSymbol identifier : providedIdentifiers)
            {
                String  name    = identifier.stringValue();
                Binding binding = identifier.resolve();

                myProvidedBindings.put(name, binding);
            }
        }

        inferName(identity.toString());
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

    /**
     * Magic for the {@link KernelModule} constructor.
     */
    void provideEverything()
    {
        assert myProvidedBindings.size() == 0;

        for (TopBinding binding : ((Namespace)myNamespace).getBindings())
        {
            String name = binding.getName();

            myProvidedBindings.put(name, binding);
        }
    }


    /**
     * @return null if the name isn't provided by this module.
     */
    Binding resolveProvidedName(String name)
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
