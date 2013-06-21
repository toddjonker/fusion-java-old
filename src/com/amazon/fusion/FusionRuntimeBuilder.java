// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

/**
 * Builder for acquiring a {@link FusionRuntime}.
 */
public class FusionRuntimeBuilder
{
    /**
     * The standard builder of {@link FusionRuntime}s, with all configuration
     * properties having their default values.
     *
     * @return a new, mutable builder instance.
     */
    public static FusionRuntimeBuilder standard()
    {
        return new FusionRuntimeBuilder.Mutable();
    }


    //=========================================================================


    private File myCurrentDirectory;
    private File[] myRepositoryDirectories;
    private boolean myDocumenting;


    private FusionRuntimeBuilder() { }

    private FusionRuntimeBuilder(FusionRuntimeBuilder that)
    {
        this.myCurrentDirectory      = that.myCurrentDirectory;
        this.myRepositoryDirectories = that.myRepositoryDirectories;
        this.myDocumenting           = that.myDocumenting;
    }


    //=========================================================================

    /**
     * Creates a mutable copy of this builder.
     *
     * @return a new builder with the same configuration as {@code this}.
     */
    public final FusionRuntimeBuilder copy()
    {
        return new FusionRuntimeBuilder.Mutable(this);
    }

    /**
     * Returns an immutable builder configured exactly like this one.
     *
     * @return this instance, if immutable;
     * otherwise an immutable copy of this instance.
     */
    public FusionRuntimeBuilder immutable()
    {
        return this;
    }

    /**
     * Returns a mutable builder configured exactly like this one.
     *
     * @return this instance, if mutable;
     * otherwise a mutable copy of this instance.
     */
    public FusionRuntimeBuilder mutable()
    {
        return copy();
    }

    void mutationCheck()
    {
        throw new UnsupportedOperationException("This builder is immutable");
    }


    //=========================================================================


    /**
     * Gets the initial value of the {@code current_directory} parameter,
     * which is the working directory for Fusion code.
     *
     * @return an absolute path. May be null, which means the builder will use
     * the {@code "user.dir"} JVM system property when {@link #build()} is
     * called.
     *
     * @see #setInitialCurrentDirectory(File)
     * @see #withInitialCurrentDirectory(File)
     */
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
     *
     * @see #getInitialCurrentDirectory()
     * @see #withInitialCurrentDirectory(File)
     */
    public void setInitialCurrentDirectory(File directory)
    {
        mutationCheck();

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


    /**
     * Declares the initial value of the {@code current_directory} parameter,
     * returning a new mutable builder if this is immutable.
     *
     * @param directory may be null, which causes the builder to use the
     * {@code "user.dir"} JVM system property when {@link #build()} is called.
     * If a relative path is given, it is immediately resolved as per
     * {@link File#getAbsolutePath()}.
     *
     * @see #getInitialCurrentDirectory()
     * @see #setInitialCurrentDirectory(File)
     */
    public final FusionRuntimeBuilder
    withInitialCurrentDirectory(File directory)
    {
        FusionRuntimeBuilder b = mutable();
        b.setInitialCurrentDirectory(directory);
        return b;
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
        mutationCheck();

        if (! directory.isAbsolute())
        {
            directory = directory.getAbsoluteFile();
        }

        if (myRepositoryDirectories == null)
        {
            myRepositoryDirectories = new File[] { directory };
        }
        else
        {
            int len = myRepositoryDirectories.length;
            myRepositoryDirectories =
                Arrays.copyOf(myRepositoryDirectories, len + 1);
            myRepositoryDirectories[len] = directory;
        }
    }

    //=========================================================================


    /** NOT FOR APPLICATION USE */
    boolean isDocumenting()
    {
        return myDocumenting;
    }

    /** NOT FOR APPLICATION USE */
    void setDocumenting(boolean documenting)
    {
        mutationCheck();
        myDocumenting = documenting;
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

            // Don't change the caller's instance
            b = b.withInitialCurrentDirectory(new File(userDir));
        }

        return b.immutable();
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


    //=========================================================================


    private static final class Mutable extends FusionRuntimeBuilder
    {
        public Mutable() { }

        public Mutable(FusionRuntimeBuilder that)
        {
            super(that);
        }

        @Override
        public FusionRuntimeBuilder immutable()
        {
            return new FusionRuntimeBuilder(this);
        }

        @Override
        public FusionRuntimeBuilder mutable()
        {
            return this;
        }

        @Override
        void mutationCheck()
        {
        }
    }
}
