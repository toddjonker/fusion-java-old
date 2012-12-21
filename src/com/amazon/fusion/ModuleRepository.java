// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
abstract class ModuleRepository
{
    /**
     * Returns human-readable text identifying this repository.
     *
     * @return not null.
     */
    abstract String identify();

    abstract ModuleIdentity resolveLib(Evaluator eval, String libName)
        throws FusionException;
}
