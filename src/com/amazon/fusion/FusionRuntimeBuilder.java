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


    public File getInitialCurrentDirectory()
    {
        return myCurrentDirectory;
    }


    /**
     * Sets the initial value of the {@code current_directory} parameter,
     * which is the working directory for Fusion code.
     *
     * @param directory may be null, which causes the builder to use the
     * {@code "user.dir"} JVM system property when {@link #build()} is called.
     * If a relative path is given, it is immediately resolved as per
     * {@link File#getAbsolutePath()}.
     */
    public void setInitialCurrentDirectory(File directory)
    {
        if (! directory.isAbsolute())
        {
            directory = directory.getAbsoluteFile();
        }
        if (! directory.isDirectory())
        {
            String message = "Argument is not a directory: " + directory;
            throw new IllegalArgumentException(message);
        }

        myCurrentDirectory = directory;
    }


    //=========================================================================


    /**
     * Declares another repository from which Fusion modules are loaded.
     * Repositories are searched in the order they are declared.
     *
     * @param directory must be a path to a readable directory.
     * If a relative path is given, it is immediately resolved as per
     * {@link File#getAbsolutePath()}.
     */
    public void addRepositoryDirectory(File directory)
    {
        if (! directory.isAbsolute())
        {
            directory = directory.getAbsoluteFile();
        }

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


    /**
     * Builds a new runtime based on the current configuration of this builder.
     *
     * @return a new builder instance.
     */
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
