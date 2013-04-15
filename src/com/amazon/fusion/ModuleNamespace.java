// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.LanguageWrap.LanguageBinding;


/**
 * Extended prepare-time {@link Namespace} that knows it's a module.
 * This exists to create special bindings that can refer module variables that
 * are not exported (but that are accessible through macro-generated code).
 */
class ModuleNamespace
    extends Namespace
{
    static final class ModuleBinding
        extends NsBinding
    {
        final ModuleIdentity myModuleId;

        private ModuleBinding(SyntaxSymbol identifier, int address,
                              ModuleIdentity moduleId)
        {
            super(identifier, address);
            myModuleId = moduleId;
        }

        @Override
        public Object lookup(Environment env)
        {
            Namespace localNamespace = env.namespace();
            if (localNamespace.getModuleId() != myModuleId)
            {
                // The local context is a different module, so we must ignore
                // it and go directly to our own namespace.

                ModuleInstance module =
                    localNamespace.getRegistry().lookup(myModuleId);
                assert module != null : "Module not found: " + myModuleId;

                ModuleStore ns = module.getNamespace();
                return ns.lookup(myAddress);
            }

            return localNamespace.lookup(this);
        }

        Object lookup(ModuleInstance module)
        {
            ModuleStore ns = module.getNamespace();

            if (module.getIdentity() != myModuleId)
            {
                module = ns.getRegistry().lookup(myModuleId);
                assert module != null : "Module not found: " + myModuleId;
                ns = module.getNamespace();
            }

            return ns.lookup(myAddress);
        }

        @Override
        public CompiledForm compileReference(Evaluator eval, Environment env)
            throws FusionException
        {
            Namespace localNamespace = env.namespace();
            if (localNamespace.getModuleId() != myModuleId)
            {
                // We have a reference to a binding from another module!
                // Compiled form must include address of the module since it
                // won't be the top of the runtime environment chain.

                int moduleAddress =
                    localNamespace.requiredModuleAddress(myModuleId);

                return new CompiledImportedVariableReference(moduleAddress,
                                                             myAddress);
            }

            return super.compileReference(eval, env);
        }

        @Override
        public String toString()
        {
            return "{{{ModuleBinding " + myModuleId + ' ' + getName() + "}}}";
        }
    }


    private final ModuleIdentity myModuleId;

    /**
     * The wraps for our used modules, then our language.  This allows us to
     * add imported bindings (via mutation of this sequence) after the wraps
     * have been propagated to existing symbols.  Which in turn means that
     * imports cover the entire module body, not just code after the import.
     */
    private final SequenceWrap myUsedModuleWraps;


    /**
     * Obnoxious helper constructor lets us allocate the sequence and retain a
     * reference to it.
     */
    private ModuleNamespace(ModuleRegistry registry, ModuleIdentity moduleId,
                            SequenceWrap wrap)
    {
        super(registry, wrap);
        myModuleId = moduleId;
        myUsedModuleWraps = wrap;
    }

    /**
     * Constructs a module with no language. Any bindings will need to be
     * {@code require}d or {@code define}d.
     *
     * @param moduleId identifies this module.
     */
    ModuleNamespace(ModuleRegistry registry, ModuleIdentity moduleId)
    {
        super(registry);
        myModuleId = moduleId;
        myUsedModuleWraps = null;
    }

    /**
     * Constructs a module with a given language.  Bindings provided by the
     * language can be shadowed by {@code require} or {@code define}.
     *
     * @param moduleId identifies this module.
     */
    ModuleNamespace(ModuleRegistry registry, ModuleInstance language,
                    ModuleIdentity moduleId)
    {
        this(registry, moduleId,
             new SequenceWrap(new LanguageWrap(language)));

        // Note that we don't add this module to the base sequence.
        // That's because it breaks {@link SyntaxWraps#stripImmediateEnvWrap}.
        // Also, this structure lets up lookup bindings in the module first
        // before proceeding on to imports and the language.
        addWrap(new EnvironmentRenameWrap(this));
    }


    @Override
    ModuleIdentity getModuleId()
    {
        return myModuleId;
    }


    @Override
    NsBinding newBinding(SyntaxSymbol identifier, int address)
    {
        return new ModuleBinding(identifier, address, myModuleId);
    }

    @Override
    public void setDoc(int address, BindingDoc doc)
    {
        doc.addProvidingModule(myModuleId);

        super.setDoc(address, doc);
    }


    @Override
    NsBinding predefine(SyntaxSymbol identifier, SyntaxValue formForErrors)
        throws FusionException
    {
        Binding oldBinding = identifier.uncachedResolveMaybe();
        if (oldBinding == null ||
            oldBinding instanceof FreeBinding ||
            oldBinding instanceof LanguageBinding)
        {
            // We need to strip off the namespace-level wrap that's already been
            // applied to the identifier. Otherwise we'll loop forever trying
            // to resolve it! This is a bit of a hack, really.
            identifier = identifier.stripImmediateEnvWrap(this);

            return addBinding(identifier);
        }

        String name = identifier.stringValue();
        throw new AmbiguousBindingFailure(null, name, formForErrors);
    }


    @Override
    void use(ModuleInstance module)
        throws FusionException
    {
        // Validate that we aren't importing a duplicate name.
        for (String name : module.providedNames())
        {
            Binding oldBinding = resolve(name);
            if (oldBinding != null
                && ! (oldBinding instanceof FreeBinding)
                && ! oldBinding.sameTarget(module.resolveProvidedName(name)))
            {
                throw new AmbiguousBindingFailure("use", name);
            }
        }

        myUsedModuleWraps.addWrap(new ModuleRenameWrap(module));
    }


    //========================================================================


    /**
     * A reference to a top-level variable in a namespace that is not the one
     * in our lexical context.
     */
    static final class CompiledImportedVariableReference
        implements CompiledForm
    {
        final int myModuleAddress;
        final int myBindingAddress;

        CompiledImportedVariableReference(int moduleAddress,
                                          int bindingAddress)
        {
            myModuleAddress  = moduleAddress;
            myBindingAddress = bindingAddress;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            NamespaceStore ns = store.namespace();
            Object result = ns.lookupImport(myModuleAddress, myBindingAddress);
            assert result != null
                : "No value for " + myModuleAddress + "@" + myBindingAddress;
            return result;
        }
    }

}
