// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;

/**
 *
 */
class FileSystemModuleRepository
    extends ModuleRepository
{
    private final File myRepoDir;

    FileSystemModuleRepository()
    {
        myRepoDir = findRepository();
    }

    /**
     * @param repoDir must be absolute.
     */
    FileSystemModuleRepository(File repoDir)
    {
        assert repoDir.isAbsolute();
        myRepoDir = repoDir;
    }


    @Override
    ModuleIdentity resolveLib(Evaluator eval, String libName)
        throws FusionException
    {
        // TODO libName should not be absolute
        String fileName = libName + ".ion"; // TODO ugly hard-coding

        File libFile = new File(myRepoDir, fileName);
        if (libFile.exists())
        {
            ModuleIdentity id = ModuleIdentity.intern(libFile);
            return id;
        }

        return null;
    }


    private static File findRepository()
    {
        // TODO this is really the wrong place to have this logic
        String fusionRepoDir = System.getProperty("com.amazon.fusion.repoDir");
        if (fusionRepoDir != null)
        {
            File repo = new File(fusionRepoDir);
            assert repo.isAbsolute()
                : "com.amazon.fusion.repoDir is not absolute: " + repo;

            return repo;
        }

        File home = findFusionHomeDir();
        File repo = new File(home, "repo");
        return repo;
    }

    private static File findFusionHomeDir()
    {
        // TODO this is really the wrong place to have this logic
        // TODO error handling
        String fusionHomeDir = System.getProperty("com.amazon.fusion.home");
        if (fusionHomeDir == null)
        {
            fusionHomeDir = System.getProperty("user.dir");
        }

        return new File(fusionHomeDir);
    }
}
