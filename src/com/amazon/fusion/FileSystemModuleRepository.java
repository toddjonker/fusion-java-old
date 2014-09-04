// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.GlobalState.FUSION_SOURCE_EXTENSION;
import static com.amazon.fusion.ModuleIdentity.isValidModuleName;
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
                private final SourceName myName =
                    SourceName.forModule(id, libFile);

                @Override
                SourceName sourceName()
                {
                    return myName;
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

    @Override
    public String toString()
    {
        return "[FileSystemModuleRepository " + identify() + ']';
    }


    private void collectModules(Predicate<ModuleIdentity> selector,
                                ModuleIdentity            parentId,
                                File                      parentDir,
                                Consumer<ModuleIdentity>  results)
    {
        String[] fileNames = parentDir.list();

        // First pass: build all "real" modules
        for (String fileName : fileNames)
        {
            if (fileName.endsWith(FUSION_SOURCE_EXTENSION))
            {
                // We assume that all .fusion files are modules.
                int endIndex =
                    fileName.length() - FUSION_SOURCE_EXTENSION.length();
                String moduleName = fileName.substring(0, endIndex);
                if (isValidModuleName(moduleName))
                {
                    ModuleIdentity child =
                        ModuleIdentity.forChild(parentId, moduleName);
                    if (selector.test(child))
                    {
                        results.accept(child);
                    }
                }
            }
        }

        // Second pass: look for directories, which are implicitly submodules.
        for (String fileName : fileNames)
        {
            File testFile = new File(parentDir, fileName);
            if (testFile.isDirectory() && isValidModuleName(fileName))
            {
                ModuleIdentity child =
                    ModuleIdentity.forChild(parentId, fileName);
                if (selector.test(child))
                {
                    // We don't add this to the results set because it may not
                    // have an associated .fusion file.
                    collectModules(selector, child, testFile, results);
                }
            }
        }

        // TODO I don't think this needs to be two passes, but I'm keeping it
        // parallel to code in ModuleDoc until determining whether that can
        // use this new approach.
    }


    @Override
    void collectModules(Predicate<ModuleIdentity> selector,
                        Consumer<ModuleIdentity>  results)
        throws FusionException
    {
        collectModules(selector, null, myRepoDir, results);
    }
}
