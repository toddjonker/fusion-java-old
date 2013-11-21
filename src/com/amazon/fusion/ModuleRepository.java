// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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
}
