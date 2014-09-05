// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.isValidAbsoluteModulePath;
import java.io.File;
import java.io.IOException;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;

/**
 * Holds configuration for code-coverage metrics collection and reporting.
 * <p>
 * Configuration is performed via a {@value #CONFIG_FILE_NAME} file
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
final class CoverageConfiguration
{
    private static final String CONFIG_FILE_NAME          = "config.properties";
    private static final String PROPERTY_INCLUDED_MODULES = "IncludedModules";
    private static final String PROPERTY_EXCLUDED_MODULES = "ExcludedModules";
    private static final String PROPERTY_INCLUDED_SOURCES = "IncludedSources";


    final Predicate<ModuleIdentity> myModuleSelector;

    /**
     * All elements must be absolute paths.
     */
    private Set<String> myIncludedSources;


    private static Set<String> readModuleSet(File       configFile,
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


    CoverageConfiguration(File dataDir)
        throws FusionException, IOException
    {
        File myConfigFile = new File(dataDir, CONFIG_FILE_NAME);
        if (myConfigFile.exists())
        {
            Properties props = FusionUtils.readProperties(myConfigFile);

            myModuleSelector =
                new SimpleModuleIdentitySelector(myConfigFile, props);

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
        else
        {
            myModuleSelector = new TrueModuleIdentitySelector();
        }
    }


    //=========================================================================


    /**
     * @return may be null.
     */
    Set<String> getIncludedSourceDirs()
    {
        return myIncludedSources;
    }


    private boolean moduleIsSelected(ModuleIdentity id)
    {
        // If no module identity, we must match a requested source directory.
        if (id == null) return false;

        return myModuleSelector.test(id);
    }


    boolean fileIsSelected(File file)
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


    boolean locationIsSelected(SourceLocation loc)
    {
        SourceName name = loc.getSourceName();
        if (name == null) return false;

        ModuleIdentity id   = name.getModuleIdentity();
        File           file = name.getFile();

        return (moduleIsSelected(id) || fileIsSelected(file));
    }


    //=========================================================================


    private static class TrueModuleIdentitySelector
        implements Predicate<ModuleIdentity>
    {
        @Override
        public boolean test(ModuleIdentity id)
        {
            return true;
        }
    }


    private static class SimpleModuleIdentitySelector
        implements Predicate<ModuleIdentity>
    {
        /**
         * All elements must be absolute module paths.
         */
        private final Set<String> myIncludedModules;

        /**
         * All elements must be absolute module paths.
         */
        private final Set<String> myExcludedModules;


        public SimpleModuleIdentitySelector(File configFile, Properties props)
            throws FusionException
        {
            myIncludedModules =
                readModuleSet(configFile, props, PROPERTY_INCLUDED_MODULES);
            myExcludedModules =
                readModuleSet(configFile, props, PROPERTY_EXCLUDED_MODULES);
        }


        private static boolean setContainsModule(Set<String> set, String path)
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


        @Override
        public boolean test(ModuleIdentity id)
        {
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
    }
}
