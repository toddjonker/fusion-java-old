// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.HashMap;
import java.util.Map;

/**
 *
 */
final class ModuleRegistry
{
    private final Map<ModuleIdentity,ModuleInstance> myModules =
        new HashMap<ModuleIdentity,ModuleInstance>();


    ModuleInstance lookup(ModuleIdentity identity)
    {
        return myModules.get(identity);
    }

    /**
     * @throws ContractFailure if a module is already registered with the same
     * {@link ModuleIdentity}.
     */
    void register(ModuleInstance instance)
        throws FusionException, ContractFailure
    {
        ModuleIdentity id = instance.getIdentity();

        ModuleInstance old = myModules.put(id, instance);
        if (old != null && old != instance)
        {
            myModules.put(id, old);
            String message =
                "Registry already has a module with identity " + id;
            throw new ContractFailure(message);
        }
    }
}
