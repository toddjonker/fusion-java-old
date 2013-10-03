// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import static com.amazon.fusion.ModuleIdentity.internFromFile;
import java.io.File;

final class FileSystemModuleRepository
    extends ModuleRepository
{
    private final File myRepoDir;

    /**
     * @param repoDir must be absolute.
     */
    FileSystemModuleRepository(File repoDir)
    {
        assert repoDir.isAbsolute();
        myRepoDir = repoDir;
    }


    @Override
    String identify()
    {
        return myRepoDir.getPath();
    }


    @Override
    ModuleIdentity resolveLib(Evaluator eval, String absoluteModulePath)
        throws FusionException
    {
        assert absoluteModulePath.startsWith("/");

        // TODO ugly hard-coding
        String fileName =
            absoluteModulePath.substring(1) + FUSION_SOURCE_EXTENSION;
        File libFile = new File(myRepoDir, fileName);
        if (libFile.exists())
        {
            return internFromFile(absoluteModulePath, libFile);
        }

        // TODO FUSION-159 remove support for .ion extension
        fileName = absoluteModulePath.substring(1) + ".ion";
        libFile = new File(myRepoDir, fileName);
        if (libFile.exists())
        {
            return internFromFile(absoluteModulePath, libFile);
        }

        return null;
    }
}
