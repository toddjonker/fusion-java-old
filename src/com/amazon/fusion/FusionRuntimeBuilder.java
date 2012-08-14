// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Builder for acquiring a {@link FusionRuntime}.
 */
public class FusionRuntimeBuilder
{
    public static FusionRuntimeBuilder standard()
    {
        return new FusionRuntimeBuilder();
    }


    //=========================================================================


    private File myCurrentDirectory;
    private List<File> myRepositoryDirectories;


    private FusionRuntimeBuilder() { }


    //=========================================================================


    public File getCurrentDirectory()
    {
        return myCurrentDirectory;
    }


    /**
     * Sets the default value of the {@code current_directory} parameter,
     * which is the working directory of all Fusion code.
     *
     * @param currentDirectory may be null, which causes the builder to use
     * the {@code "user.dir"} JVM system property.
     */
    public void setCurrentDirectory(File currentDirectory)
    {
        if (currentDirectory != null && ! currentDirectory.isDirectory())
        {
            String message =
                "currentDirectory is not a directory: " + currentDirectory;
            throw new IllegalArgumentException(message);
        }

        myCurrentDirectory = currentDirectory;
    }


    //=========================================================================


    /**
     * Repositories are searched in the order they are declared.
     *
     * @param directory must be a valid, readable directory.
     */
    public void addRepositoryDirectory(File directory)
    {
        if (myRepositoryDirectories == null)
        {
            myRepositoryDirectories = new ArrayList<File>();
        }
        myRepositoryDirectories.add(directory);
    }


    //=========================================================================


    private FusionRuntimeBuilder fillDefaults()
    {
        FusionRuntimeBuilder b = this;
        if (b.myCurrentDirectory == null)
        {
            String userDir = System.getProperty("user.dir", "");
            if (userDir.isEmpty())
            {
                String message =
                    "Unable to determine working directory: " +
                    "the JDK system property user.dir is not set.";
                throw new IllegalStateException(message);
            }

            // TODO copy the builder so user's instance isn't changed.
            b.myCurrentDirectory = new File(userDir);
        }

        // TODO make immutable? Currently we don't retain this instance so
        // it doesn't matter.
        return b;
    }


    public FusionRuntime build()
    {
        FusionRuntimeBuilder b = fillDefaults();
        return new StandardRuntime(b);
    }


    /**
     * NOT PUBLIC!
     */
    ModuleRepository[] buildModuleRepositories()
    {
        ArrayList<ModuleRepository> repos = new ArrayList<ModuleRepository>();
        repos.add(new JarModuleRepository());

        if (myRepositoryDirectories != null)
        {
            for (File f : myRepositoryDirectories)
            {
                repos.add(new FileSystemModuleRepository(f));
            }
        }

        return repos.toArray(new ModuleRepository[repos.size()]);
    }
}
