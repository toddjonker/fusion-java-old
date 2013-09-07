// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.HashMap;
import java.util.Map;

/**
 * This class must be thread-safe.
 */
final class ModuleRegistry
{
    private final Map<ModuleIdentity,ModuleInstance> myModules =
        new HashMap<ModuleIdentity,ModuleInstance>();


    /**
     * Finds a module in this registry.
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
