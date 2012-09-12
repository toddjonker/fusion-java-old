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
    class ModuleTopBinding extends NsBinding
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
                return ModuleNamespace.this.lookup(this);
            }

            return localNamespace.lookup(this);
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
}
