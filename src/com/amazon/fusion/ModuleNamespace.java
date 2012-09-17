// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Extended {@link Namespace} that knows it's a module.
 * This exists to create special bindings that can refer module variables that
 * are not exported (but that are accessible through macro-generated code).
 */
class ModuleNamespace
    extends Namespace
{
    /** These are compiled away, unlike some bindings. */
    static final class ModuleTopBinding extends NsBinding
    {
        private final ModuleIdentity myModuleId;

        private ModuleTopBinding(SyntaxSymbol identifier, int address,
                                 ModuleIdentity moduleId)
        {
            super(identifier, address);
            myModuleId = moduleId;
        }

        ModuleIdentity getModuleId()
        {
            return myModuleId;
        }


        @Override
        public FusionValue lookup(Environment env)
        {
            Namespace localNamespace = env.namespace();
            if (localNamespace.getModuleId() != myModuleId)
            {
                // The local context is a different module, so we must ignore
                // it and go directly to our own namespace.
                // This case doesn't happen at runtime, instead the binding
                // is compiled into a special form.

                ModuleInstance module =
                    localNamespace.getRegistry().lookup(myModuleId);
                assert module != null : "Module not found: " + myModuleId;

                ModuleNamespace ns = module.getNamespace();
                return ns.lookup(this);
            }

            return localNamespace.lookup(this);
        }

        @Override
        public CompiledForm compile(Evaluator eval, Environment env)
            throws FusionException
        {
            Namespace localNamespace = env.namespace();
            if (localNamespace.getModuleId() != myModuleId)
            {
                // We have a reference to a binding from another module!
                // Compiled form must include link to the module since it
                // won't be the top of the runtime environment chain.

                ModuleInstance module =
                    localNamespace.getRegistry().lookup(myModuleId);
                assert module != null : "Module not found: " + myModuleId;

                ModuleNamespace ns = module.getNamespace();
                return new CompiledImportedVariable(ns, myAddress);
            }

            return super.compile(eval, env);
        }

        @Override
        public String toString()
        {
            return "{{ModuleTopBinding " + getIdentifier() + "}}";
        }
    }


    private final ModuleIdentity myModuleId;

    ModuleNamespace(ModuleRegistry registry, ModuleIdentity moduleId)
    {
        super(registry);
        myModuleId = moduleId;
    }

    ModuleNamespace(ModuleRegistry registry, ModuleInstance language,
                    ModuleIdentity moduleId)
    {
        super(registry, language);
        myModuleId = moduleId;
    }


    @Override
    ModuleIdentity getModuleId()
    {
        return myModuleId;
    }


    @Override
    NsBinding newBinding(SyntaxSymbol identifier, int address)
    {
        return new ModuleTopBinding(identifier, address, myModuleId);
    }


    //========================================================================


    /**
     * A reference to a top-level binding in a namespace that is not the one
     * in our lexical context.
     */
    private static final class CompiledImportedVariable
        implements CompiledForm
    {
        private final ModuleNamespace myNamespace;
        private final int             myAddress;

        CompiledImportedVariable(ModuleNamespace namespace, int address)
        {
            myNamespace = namespace;
            myAddress   = address;
        }

        @Override
        public Object doEval(Evaluator eval, Store store)
            throws FusionException
        {
            Object result = myNamespace.lookup(myAddress);
            assert result != null : "No value for " + myAddress;
            return result;
        }
    }
}
