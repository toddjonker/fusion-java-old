// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
class JarModuleRepository
    extends ModuleRepository
{
    @Override
    ModuleIdentity resolveLib(Evaluator eval, String libName)
        throws FusionException
    {
        // TODO libName should not be absolute
        String fileName = libName + ".ion"; // TODO ugly hard-coding

        if (getClass().getResource("/FUSION-REPO/" + fileName) != null)
        {
            ModuleIdentity id = ModuleIdentity.internFromJar(fileName);
            return id;
        }
        return null;
    }
}
