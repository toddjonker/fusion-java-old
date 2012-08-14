// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
abstract class ModuleRepository
{

    abstract ModuleIdentity resolveLib(Evaluator eval, String libName)
        throws FusionException;
}
