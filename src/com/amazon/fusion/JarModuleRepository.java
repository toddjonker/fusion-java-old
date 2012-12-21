// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

final class JarModuleRepository
    extends ModuleRepository
{
    @Override
    String identify()
    {
        return "ClassLoader repository";
    }


    @Override
    ModuleIdentity resolveLib(Evaluator eval, String libName)
        throws FusionException
    {
        assert libName.startsWith("/");

        // TODO absolute vs relative paths
        String fileName = libName.substring(1) + ".ion"; // TODO ugly hard-coding

        if (getClass().getResource("/FUSION-REPO/" + fileName) != null)
        {
            ModuleIdentity id = ModuleIdentity.internFromJar(fileName);
            return id;
        }
        return null;
    }
}
