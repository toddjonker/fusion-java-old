// Copyright (c) 2012-2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionSymbol.BaseSymbol;
import com.amazon.fusion.LanguageWrap.LanguageBinding;
import com.amazon.fusion.TopLevelNamespace.TopLevelBinding;
import java.util.Iterator;
import java.util.Set;


/**
 * Extended prepare-time {@link Namespace} that knows it's a module.
 * This exists to create special bindings that can refer module variables that
 * are not exported (but that are accessible through macro-generated code).
 */
final class ModuleNamespace
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
        public Object lookup(Namespace localNamespace)
        {
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

            // We can't use our address directly, since we may be compiling
            // and the binding may not have a location allocated yet.
            return localNamespace.lookupDefinition(this);
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
        CompiledForm compileDefine(Evaluator eval,
                                   Environment env,
                                   SyntaxSymbol id,
                                   CompiledForm valueForm)
            throws FusionException
        {
            return env.namespace().compileDefine(eval, this, id, valueForm);
        }

        @Override
        public CompiledForm compileTopReference(Evaluator eval,
                                                Environment env,
                                                SyntaxSymbol id)
            throws FusionException
        {
            // We should never get here.
            String message =
                "#%top not implemented for module binding: " + this;
            throw new SyntaxException("#%top", message, id);
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

            return compileLocalTopReference(eval, env);
        }

        @Override
        public String toString()
        {
            return "{{{ModuleBinding " + myModuleId.absolutePath()
                + ' ' + getIdentifier().debugString() + "}}}";
        }
    }


    private static final class ModuleWrap
        extends EnvironmentRenameWrap
    {
        ModuleWrap(ModuleNamespace ns)
        {
            super(ns);
        }

        @Override
        Binding resolveTop(BaseSymbol name,
                           Iterator<SyntaxWrap> moreWraps,
                           Set<Integer> returnMarks)
        {
            if (moreWraps.hasNext())
            {
                SyntaxWrap nextWrap = moreWraps.next();
                return nextWrap.resolve(name, moreWraps, returnMarks);
            }
            return null;
        }

        @Override
        public String toString()
        {
            ModuleIdentity id =
                ((ModuleNamespace) getEnvironment()).getModuleId();
            return "{{{ModuleWrap " + id.absolutePath() + "}}}";
        }
    }



    /**
     * The wraps for our used modules, then our language.  This allows us to
     * add imported bindings (via mutation of this sequence) after the sequence
     * has been propagated to existing identifiers.  Which in turn means that
     * imports cover the entire module body, not just code after the import.
     */
    private final SequenceWrap myUsedModuleWraps;


    /**
     * Obnoxious helper constructor lets us allocate the sequence and retain a
     * reference to it.
     * <p>
     * Note that we don't add this module to the base sequence: as we encounter
     * require forms, new entries will be added to {@link #myUsedModuleWraps},
     * and all required modules need to fall after this module in the overall
     * sequence of wraps on the namespace. That ensures that bindings in this
     * module have precedence over bindings imported from other modules.
     */
    private ModuleNamespace(ModuleRegistry registry, ModuleIdentity moduleId,
                            final SequenceWrap usedModuleWraps)
    {
        super(registry, moduleId,
              new Function<Namespace, SyntaxWraps>()
              {
                  @Override
                  public SyntaxWraps apply(Namespace _this) {
                      ModuleNamespace __this = (ModuleNamespace) _this;
                      return SyntaxWraps.make(new ModuleWrap(__this),
                                              usedModuleWraps);
                  }
              });
        myUsedModuleWraps = usedModuleWraps;
    }

    /**
     * Constructs a module that uses no other module. Any bindings will need to
     * be created via {@link #bind(String, Object)}.
     *
     * @param moduleId identifies this module.
     */
    ModuleNamespace(ModuleRegistry registry, ModuleIdentity moduleId)
    {
        super(registry, moduleId,
              new Function<Namespace, SyntaxWraps>()
              {
                  @Override
                  public SyntaxWraps apply(Namespace _this) {
                      return SyntaxWraps.make(new EnvironmentRenameWrap(_this));
                  }
              });
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
    }


    @Override
    NsBinding newBinding(SyntaxSymbol identifier, int address)
    {
        return new ModuleBinding(identifier, address, getModuleId());
    }

    @Override
    public void setDoc(int address, BindingDoc doc)
    {
        doc.addProvidingModule(getModuleId());

        super.setDoc(address, doc);
    }


    @Override
    SyntaxSymbol predefine(SyntaxSymbol identifier, SyntaxValue formForErrors)
        throws FusionException
    {
        // Don't cache the binding! The symbol instance may also be in use as
        // a reference to this binding (due to a macro expansion), in which
        // case this result is not correct for that reference.
        Binding oldBinding = identifier.uncachedResolveMaybe();
        if (oldBinding == null ||
            oldBinding instanceof FreeBinding)
        {
            identifier = identifier.copyAndResolveTop();
        }
        else if (oldBinding instanceof LanguageBinding)
        {
            // Visible binding is from our language, we can shadow it.
            // Again, be careful not to cache a binding in the original id.
            identifier = identifier.copyReplacingBinding(oldBinding);
        }
        else // there's an imported binding
        {
            String name = identifier.stringValue();
            throw new AmbiguousBindingFailure(null, name, formForErrors);
        }

        NsBinding b = addBinding(identifier);
        return identifier.copyReplacingBinding(b);
    }


    @Override
    void require(ModuleInstance module)
        throws FusionException
    {
        // Validate that we aren't importing a duplicate name.
        for (String name : module.providedNames())
        {
            Binding oldBinding = resolve(name);
            if (oldBinding != null
                && ! (oldBinding instanceof FreeBinding)
                && ! (oldBinding instanceof LanguageBinding)
                && ! oldBinding.sameTarget(module.resolveProvidedName(name)))
            {
                throw new AmbiguousBindingFailure(GlobalState.REQUIRE, name);
            }
        }

        myUsedModuleWraps.addWrap(new ModuleRenameWrap(module));
    }


    @Override
    CompiledForm compileDefine(Evaluator eval,
                               FreeBinding binding,
                               SyntaxSymbol id,
                               CompiledForm valueForm)
        throws FusionException
    {
        throw new IllegalStateException("Unexpected define in module: "
                                        + binding);
    }


    @Override
    CompiledForm compileDefine(Evaluator eval,
                               TopLevelBinding binding,
                               SyntaxSymbol id,
                               CompiledForm valueForm)
        throws FusionException
    {
        throw new IllegalStateException("Unexpected define in module: "
                                        + binding);
    }


    @Override
    CompiledForm compileDefine(Evaluator eval,
                               ModuleBinding binding,
                               SyntaxSymbol id,
                               CompiledForm valueForm)
        throws FusionException
    {
        String name = binding.getName().stringValue();
        return new CompiledTopDefine(name, binding.myAddress, valueForm);

    }


    @Override
    CompiledForm compileFreeTopReference(SyntaxSymbol identifier)
        throws FusionException
    {
        throw new IllegalStateException("Unexpected #%top in module: "
                                        + identifier);
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
