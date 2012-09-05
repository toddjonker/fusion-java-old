// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Environment.Binding;
import com.amazon.fusion.Namespace.NsBinding;
import com.amazon.ion.util.IonTextUtils;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * A module that's been instantiated for use by one or more other modules.
 * A module has a unique {@link ModuleIdentity} and a {@link Namespace}
 * holding its top-level bindings.
 */
class ModuleInstance
    extends NamedValue
{
    class ModuleBinding
        implements Binding
    {
        private final String myName;
        private final NsBinding myInternalBinding;

        private ModuleBinding(String name, NsBinding binding)
        {
            myName = name;
            myInternalBinding = binding;
            assert myInternalBinding != null;
        }

        ModuleInstance getModule()
        {
            return ModuleInstance.this;
        }

        String getName()
        {
            return myName;
        }


        @Override
        public Binding originalBinding()
        {
            return myInternalBinding;
        }


        @Override
        public FusionValue lookup(Environment store)
        {
            // TODO optimize this path
            return myNamespace.lookup(myInternalBinding);
        }

        @Override
        public boolean equals(Object other)
        {
            return this == other;
        }

        @Override
        public String toString()
        {
            return "{{ModuleBinding " + myIdentity + ' ' + myName + "}}";
        }
    }

    private final ModuleIdentity myIdentity;
    private final Namespace myNamespace;
    private final Map<String,ModuleBinding> myProvidedBindings;

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
                   SyntaxSymbol[] providedIdentifiers)
        throws FusionException, ContractFailure
    {
        myIdentity = identity;
        myNamespace = namespace;

        if (providedIdentifiers == null)
        {
            myProvidedBindings = new HashMap<String,ModuleBinding>();
        }
        else
        {
            myProvidedBindings =
                new HashMap<String,ModuleBinding>(providedIdentifiers.length);

            for (SyntaxSymbol identifier : providedIdentifiers)
            {
                String name = identifier.stringValue();

                ModuleBinding externalBinding;
                Binding internalBinding = identifier.resolve();
                if (internalBinding instanceof ModuleBinding)
                {
                    // Export of a binding from another module, just use it.
                    externalBinding = (ModuleBinding) internalBinding;
                }
                else
                {
                    externalBinding =
                        new ModuleBinding(name, (NsBinding) internalBinding);
                }

                myProvidedBindings.put(name, externalBinding);
            }
        }

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

    /**
     * Magic for the {@link KernelModule} constructor.
     */
    void provideEverything()
    {
        assert myProvidedBindings.size() == 0;

        for (NsBinding internalBinding : myNamespace.getBindings())
        {
            String name = internalBinding.getIdentifier().stringValue();

            ModuleBinding externalBinding =
                new ModuleBinding(name, internalBinding);

            myProvidedBindings.put(name, externalBinding);
        }
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
