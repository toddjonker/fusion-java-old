// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Provides access to module source code in some (usually persistent) store.
 */
abstract class ModuleRepository
{
    /**
     * Returns human-readable text identifying this repository.
     *
     * @return not null.
     */
    abstract String identify();

    abstract ModuleLocation locateModule(Evaluator eval, ModuleIdentity id)
        throws FusionException;

    /**
     * Enumerate modules that are visible to this repository.
     * This may not be the entire set of loadable modules! Some repositories
     * may not be able to enumerate their own content, and submodules may not
     * be discovered until their containing module is loaded.
     */
    abstract void collectModules(Predicate<ModuleIdentity> selector,
                                 Consumer<ModuleIdentity>  results)
        throws FusionException;
}
