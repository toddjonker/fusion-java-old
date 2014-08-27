// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.isValidAbsoluteModulePath;
import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

/**
 * Implements code-coverage metrics collection.
 * <p>
 * The collector is given a data directory from which it reads configuration
 * and where it persists its metrics database.  This allows multiple runtime
 * launches to contribute to the same set of metrics.  That's common during
 * unit testing where each test case uses a fresh {@link FusionRuntime}.
 * <p>
 * At present, only file-based sources are instrumented. This includes sources
 * loaded from a file-based {@link ModuleRepository} as well as scripts from
 * other locations.
 * <p>
 * Metrics configuration is performed via a {@value #CONFIG_FILE_NAME} file
 * inside the data directory. This makes it easy to configure from external
 * build scripts.
 *
 * <h2>Coverage Filtering</h2>
 *
 * The collector can be configured to select what Fusion code is instrumented.
 * Coverage filtering is based on three configuration properties:
 * <ul>
 *   <li>{@value #PROPERTY_INCLUDED_MODULES} holds a list of absolute module
 *       paths, separated by ':'.
 *       Any repository-loaded module at or under a given path is instrumented
 *       (unless excluded).
 *       If not provided, all repository-loaded modules are included.
 *       If empty, then no repository-loaded modules are instrumented.
 *   <li>{@value #PROPERTY_EXCLUDED_MODULES} holds a list of absolute module
 *       paths, separated by ':'.
 *       Any repository-loaded module, included with respect to
 *       {@value #PROPERTY_INCLUDED_MODULES}, that is at or under a given path
 *       is <em>not</em> instrumented.
 *       If empty or not provided, all included modules are instrumented.
 *   <li>{@value #PROPERTY_INCLUDED_SOURCES} holds a list of directory paths,
 *       separated by the platform path separator.
 *       All code read from files under a given directory is instrumented.
 *       If not provided, only modules chosen by IncludedModules are
 *       instrumented.
 * <ul>
 */
public final class _Private_CoverageCollectorImpl
    implements _Private_CoverageCollector
{

    private static final String CONFIG_FILE_NAME          = "config.properties";
    private static final String PROPERTY_INCLUDED_MODULES = "IncludedModules";
    private static final String PROPERTY_EXCLUDED_MODULES = "ExcludedModules";
    private static final String PROPERTY_INCLUDED_SOURCES = "IncludedSources";


    /** Where we store our metrics. */
    private final CoverageDatabase myDatabase;

    /**
     * All elements must be absolute module paths.
     */
    private Set<String> myIncludedModules;

    /**
     * All elements must be absolute module paths.
     */
    private Set<String> myExcludedModules;

    /**
     * All elements must be absolute paths.
     */
    private Set<String> myIncludedSources;


    private Set<String> readModuleSet(File       configFile,
                                      Properties props,
                                      String     propertyName)
        throws FusionException
    {
        String mods = props.getProperty(propertyName);
        if (mods == null) return null;

        Set<String> modules = new HashSet<>();

        for (String mod : mods.split(":"))
        {
            if (mod.isEmpty()) continue;

            if (isValidAbsoluteModulePath(mod))
            {
                modules.add(mod);
            }
            else
            {
                String message =
                    "Configuration error in " + configFile
                    + ": " + propertyName
                    + " contains an invalid absolute module path: " + mod;
                throw new FusionException(message);
            }
        }

        return modules;
    }


    private _Private_CoverageCollectorImpl(File dataDir)
        throws FusionException, IOException
    {
        myDatabase = new CoverageDatabase(dataDir);

        File myConfigFile = new File(dataDir, CONFIG_FILE_NAME);
        if (myConfigFile.exists())
        {
            Properties props = FusionUtils.readProperties(myConfigFile);

            myIncludedModules =
                readModuleSet(myConfigFile, props, PROPERTY_INCLUDED_MODULES);
            myExcludedModules =
                readModuleSet(myConfigFile, props, PROPERTY_EXCLUDED_MODULES);

            String sources = props.getProperty(PROPERTY_INCLUDED_SOURCES);
            if (sources != null)
            {
                myIncludedSources = new HashSet<>();

                String pathSeparator = System.getProperty("path.separator");
                String fileSeparator = System.getProperty("file.separator");

                for (String source : sources.split(pathSeparator))
                {
                    if (source.isEmpty()) continue;

                    File f = new File(source);
                    if (f.isDirectory())
                    {
                        String path = f.getAbsolutePath() + fileSeparator;
                        myIncludedSources.add(path);
                    }
                    else
                    {
                        String message =
                            "Configuration error in " + myConfigFile
                            + ": " + PROPERTY_INCLUDED_SOURCES
                            + " contains an invalid directory: " + source;
                        throw new FusionException(message);
                    }
                }
            }
        }
    }



    public static _Private_CoverageCollectorImpl fromDirectory(File dataDir)
        throws FusionException
    {
        try
        {
            return new _Private_CoverageCollectorImpl(dataDir);
        }
        catch (IOException e)
        {
            throw new FusionException("Error reading coverage data", e);
        }
    }

    public static _Private_CoverageCollectorImpl fromDirectory(String dataDir)
        throws FusionException
    {
        return fromDirectory(new File(dataDir));
    }


    //=========================================================================
    // Managing the coverage data


    private boolean setContainsModule(Set<String> set, String path)
    {
        for (String coverable : set)
        {
            if (path.equals(coverable) || path.startsWith(coverable + '/'))
            {
                return true;
            }
        }

        return false;
    }


    private boolean moduleIsCoverable(ModuleIdentity id)
    {
        // If no module identity, we must match a requested source directory.
        if (id == null) return false;

        String path = id.absolutePath();

        // By default, all modules are included.
        if (myIncludedModules != null)
        {
            if (! setContainsModule(myIncludedModules, path)) return false;
        }

        if (myExcludedModules != null)
        {
            if (setContainsModule(myExcludedModules, path)) return false;
        }

        return true;
    }


    private boolean fileIsCoverable(File file)
    {
        if (file != null)
        {
            // We don't cover a file unless it's explicitly included.
            if (myIncludedSources == null) return false;

            String path = file.getAbsolutePath();

            for (String coverable : myIncludedSources)
            {
                if (path.startsWith(coverable)) return true;
            }
        }

        return false;
    }


    private boolean isIncluded(SourceLocation loc)
    {
        SourceName name = loc.getSourceName();
        if (name == null) return false;

        ModuleIdentity id   = name.getModuleIdentity();
        File           file = name.getFile();

        return (moduleIsCoverable(id) || fileIsCoverable(file));
    }


    @Override
    public boolean coverableLocation(SourceLocation loc)
    {
        boolean coverable = isIncluded(loc);

        if (coverable)
        {
            myDatabase.noteCoverableLocation(loc);
        }

        return coverable;
    }

    @Override
    public void coverLocation(SourceLocation loc)
    {
        myDatabase.coverLocation(loc);
    }


    @Override
    public void flushMetrics()
        throws FusionException
    {
        try
        {
            myDatabase.write();
        }
        catch (IOException e)
        {
            throw new FusionException("Error writing coverage data", e);
        }
    }
}
