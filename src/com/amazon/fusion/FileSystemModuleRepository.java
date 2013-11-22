// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

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
    ModuleLocation locateModule(Evaluator eval, final ModuleIdentity id)
        throws FusionException
    {
        String path = id.absolutePath();
        String fileName = path.substring(1) + FUSION_SOURCE_EXTENSION;

        final File libFile = new File(myRepoDir, fileName);
        if (libFile.exists())
        {
            ModuleLocation loc = new InputStreamModuleLocation()
            {
                @Override
                SourceName sourceName()
                {
                    String name = id + " (at file:" + libFile + ")";
                    return SourceName.forDisplay(name);
                }

                @Override
                InputStream open()
                    throws IOException
                {
                    return new FileInputStream(libFile);
                }

                @Override
                String parentDirectory()
                {
                    return libFile.getParentFile().getAbsolutePath();
                }
            };

            return loc;
        }

        return null;
    }
}
