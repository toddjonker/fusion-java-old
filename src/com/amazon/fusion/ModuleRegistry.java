// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ModuleForm.CompiledModule;
import java.util.HashMap;
import java.util.Map;

/**
 * This class must be thread-safe.
 */
final class ModuleRegistry
{
    private final Map<ModuleIdentity,CompiledModule> myDeclarations =
        new HashMap<>();

    // TODO FUSION-32 this should separate module instances by phase
    private final Map<ModuleIdentity,ModuleInstance> myModules =
        new HashMap<>();


    /**
     * Finds a module instance that's already in this registry.
     *
     * @param identity the desired module
     *
     * @return null if the module doesn't exist in this registry.
     */
    synchronized ModuleInstance lookup(ModuleIdentity identity)
    {
        return myModules.get(identity);
    }


    /**
     * Determines whether a module has been declared or instantiated in this
     * registry.
     */
    synchronized boolean isLoaded(ModuleIdentity identity)
    {
        return myDeclarations.containsKey(identity)
            || myModules.containsKey(identity);
    }


    /**
     * @return null if the module has not been instantiated and has not been
     * declared.
     */
    synchronized ModuleInstance instantiate(Evaluator eval,
                                            ModuleIdentity identity)
        throws FusionException
    {
        ModuleInstance instance = myModules.get(identity);
        if (instance == null)
        {
            CompiledModule decl = myDeclarations.get(identity);
            if (decl != null)
            {
                instance = decl.instantiate(eval);
                myModules.put(identity, instance);
            }
        }
        return instance;
    }


    /**
     * @throws ContractException if a module is already declared with the
     * same {@link ModuleIdentity}.
     */
    synchronized void declare(ModuleIdentity id, CompiledModule decl)
        throws FusionException, ContractException
    {
        CompiledModule old = myDeclarations.put(id, decl);
        if (old != null && old != decl)
        {
            myDeclarations.put(id, old);
            String message =
                "Registry already has a module declared with identity " + id;
            throw new ContractException(message);
        }
    }


    /**
     * @throws ContractException if a module is already registered with the
     * same {@link ModuleIdentity}.
     */
    synchronized void register(ModuleInstance instance)
        throws FusionException, ContractException
    {
        ModuleIdentity id = instance.getIdentity();

        ModuleInstance old = myModules.put(id, instance);
        if (old != null && old != instance)
        {
            myModules.put(id, old);
            String message =
                "Registry already has a module with identity " + id;
            throw new ContractException(message);
        }
    }
}
