// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.BindingDoc.Kind;
import com.amazon.fusion.Namespace.NsBinding;
import java.util.Collection;


final class ModuleBuilderImpl
    implements ModuleBuilder
{
    private final ModuleNameResolver myResolver;
    private final ModuleRegistry     myRegistry;
    private final ModuleNamespace    myNamespace;

    /**
     * Prepares to build a module with no language.
     */
    ModuleBuilderImpl(ModuleNameResolver resolver,
                      ModuleRegistry registry,
                      ModuleIdentity moduleId)
    {
        myResolver = resolver;
        myRegistry = registry;
        myNamespace = new ModuleNamespace(registry, moduleId);
    }

    @Override
    public void define(String name, Object value)
        throws FusionException
    {
        myNamespace.bind(name, value);

        if (value instanceof BaseValue)
        {
            BindingDoc doc = ((BaseValue) value).document();
            if (doc != null)
            {
                myNamespace.setDoc(name, doc);
            }
        }
    }

    void define(String name, Object value, String documentation)
        throws FusionException
    {
        myNamespace.bind(name, value);

        Kind kind =
            (value instanceof Procedure ? Kind.PROCEDURE : Kind.CONSTANT);
        myNamespace.setDoc(name, kind, documentation);
    }


    @Override
    public void instantiate()
        throws FusionException
    {
        ModuleStore store = new ModuleStore(myRegistry,
                                            myNamespace.extractValues(),
                                            myNamespace.extractBindingDocs());
        Collection<NsBinding> bindings = myNamespace.getBindings();

        ModuleIdentity id = myNamespace.getModuleId();

        ModuleInstance module = new ModuleInstance(id, store, bindings);

        myRegistry.register(module);

        myResolver.registerDeclaredModule(myRegistry, id);
    }
}
