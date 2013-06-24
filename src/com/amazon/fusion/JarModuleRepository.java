// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import static com.amazon.fusion.ModuleIdentity.internFromClasspath;

final class JarModuleRepository
    extends ModuleRepository
{
    @Override
    String identify()
    {
        return "ClassLoader repository";
    }


    @Override
    ModuleIdentity resolveLib(Evaluator eval, String absoluteModulePath)
        throws FusionException
    {
        assert absoluteModulePath.startsWith("/");

        // TODO ugly hard-coding
        String fileName =
            "/FUSION-REPO" + absoluteModulePath + FUSION_SOURCE_EXTENSION;
        if (getClass().getResource(fileName) != null)
        {
            return internFromClasspath(absoluteModulePath, fileName);
        }

        // TODO FUSION-159 remove support for .ion extension
        fileName = "/FUSION-REPO" + absoluteModulePath + ".ion";
        if (getClass().getResource(fileName) != null)
        {
            return internFromClasspath(absoluteModulePath, fileName);
        }

        return null;
    }
}
