// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

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

        // TODO FUSION-79 absolute vs relative paths
        String fileName = "/FUSION-REPO" + libName + ".ion"; // TODO ugly hard-coding

        if (getClass().getResource(fileName) != null)
        {
            ModuleIdentity id = ModuleIdentity.internFromClasspath(libName,
                                                                   fileName);
            return id;
        }
        return null;
    }
}
