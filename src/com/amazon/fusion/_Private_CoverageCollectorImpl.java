// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.ModuleIdentity.isValidAbsoluteModulePath;
import static com.amazon.ion.IonType.LIST;
import static com.amazon.ion.IonType.STRUCT;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonWriter;
import com.amazon.ion.system.IonSystemBuilder;
import com.amazon.ion.system.IonTextWriterBuilder;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
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


    private final File myDataDir;
    private final File myCoverageFile;

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

    private final Map<SourceLocation,Boolean> myLocations = new HashMap<>();


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
        myDataDir = dataDir;
        myCoverageFile = new File(myDataDir, "coverage.ion");

        File myConfigFile = new File(myDataDir, CONFIG_FILE_NAME);
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

        if (myCoverageFile.exists())
        {
            readFrom(myCoverageFile);
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
            synchronized (this)
            {
                Boolean prev = myLocations.put(loc, Boolean.FALSE);

                // If already covered, don't un-cover it!
                if (prev != null && prev)
                {
                    myLocations.put(loc, prev);
                }
            }
        }

        return coverable;
    }

    @Override
    public synchronized void coverLocation(SourceLocation loc)
    {
        myLocations.put(loc, Boolean.TRUE);
    }


    /** Has a location been covered? */
    synchronized boolean locationCovered(SourceLocation loc)
    {
        Boolean covered = myLocations.get(loc);
        return (covered != null && covered);
    }


    @Override
    public void flushMetrics()
        throws FusionException
    {
        try
        {
            writeTo(myCoverageFile);
        }
        catch (IOException e)
        {
            throw new FusionException("Error writing coverage data", e);
        }
    }


    //=========================================================================


    private static final class SourceNameComparator
        implements Comparator<SourceName>
    {
        @Override
        public int compare(SourceName a, SourceName b)
        {
            return a.display().compareTo(b.display());
        }
    }

    static final Comparator<SourceName> SRCNAME_COMPARE =
        new SourceNameComparator();


    private static final class SourceLocationComparator
        implements Comparator<SourceLocation>
    {
        @Override
        public int compare(SourceLocation a, SourceLocation b)
        {
            if (a.getLine() < b.getLine()) return -1;
            if (a.getLine() > b.getLine()) return  1;

            if (a.getColumn() < b.getColumn()) return -1;
            if (a.getColumn() > b.getColumn()) return  1;

            return 0;
        }
    }

    static final Comparator<SourceLocation> SRCLOC_COMPARE =
        new SourceLocationComparator();



    //=========================================================================


    synchronized Set<SourceName> sourceNames()
    {
        Set<SourceName> names = new HashSet<>();

        Iterator<SourceLocation> i = myLocations.keySet().iterator();
        while (i.hasNext())
        {
            SourceLocation loc = i.next();
            if (loc.myName != null)
            {
                names.add(loc.myName);
            }
        }

        return names;
    }


    SourceName[] sortedNames()
    {
        Set<SourceName> sourceSet = sourceNames();

        SourceName[] sourceArray = sourceSet.toArray(new SourceName[0]);

        Arrays.sort(sourceArray, SRCNAME_COMPARE);

        return sourceArray;
    }


    synchronized SourceLocation[] sortedLocations(SourceName name)
    {
        ArrayList<SourceLocation> locsList = new ArrayList<>();

        Iterator<SourceLocation> i = myLocations.keySet().iterator();
        while (i.hasNext())
        {
            SourceLocation loc = i.next();
            if (name.equals(loc.myName))
            {
                locsList.add(loc);
            }
        }

        SourceLocation[] locsArray =
            locsList.toArray(new SourceLocation[locsList.size()]);

        Arrays.sort(locsArray, SRCLOC_COMPARE);

        return locsArray;
    }


    synchronized SourceLocation[] sortedLocations(File sourceFile)
    {
        SourceName name = SourceName.forFile(sourceFile);
        return sortedLocations(name);
    }


    //=========================================================================


    private void writeSourceName(IonWriter iw, SourceName name)
        throws IOException
    {
        iw.stepIn(STRUCT);
        {
            iw.setFieldName("file");
            iw.writeString(name.getFile().getPath());

            ModuleIdentity id = name.getModuleIdentity();
            if (id != null)
            {
                iw.setFieldName("module");
                iw.writeString(id.absolutePath());
            }
        }
        iw.stepOut();
    }


    private void writeLocation(IonWriter iw, SourceLocation loc)
        throws IOException
    {
        iw.stepIn(STRUCT);
        {
            long line = loc.getLine();
            long col  = loc.getColumn();

            boolean covered = locationCovered(loc);

            iw.setFieldName("line");
            iw.writeInt(line);

            iw.setFieldName("column");
            iw.writeInt(col);

            iw.setFieldName("covered");
            iw.writeBool(covered);
        }
        iw.stepOut();
    }


    private void writeLocations(IonWriter iw, SourceName name)
        throws IOException
    {
        SourceLocation[] locations = sortedLocations(name);
        assert locations.length != 0;

        iw.stepIn(LIST);
        {
            for (SourceLocation loc : locations)
            {
                writeLocation(iw, loc);
            }
        }
        iw.stepOut();
    }


    private void writeSource(IonWriter iw, SourceName name)
        throws IOException
    {
        iw.stepIn(STRUCT);
        {
            iw.setFieldName("name");
            writeSourceName(iw, name);

            iw.setFieldName("locations");
            writeLocations(iw, name);
        }
        iw.stepOut();
    }


    private void writeTo(IonWriter iw)
        throws IOException
    {
        for (SourceName name : sourceNames())
        {
            if (name.getFile() != null)
            {
                writeSource(iw, name);
            }
        }
    }


    private void writeTo(OutputStream out)
        throws IOException
    {
        try (IonWriter iw = IonTextWriterBuilder.minimal().build(out))
        {
            writeTo(iw);
        }
    }


    private void writeTo(File where)
        throws IOException
    {
        FusionUtils.createParentDirs(where);
        try (OutputStream out = new FileOutputStream(where))
        {
            writeTo(out);
        }
    }


    //=========================================================================


    private SourceName readSourceName(IonReader in)
        throws IOException
    {
        SourceName name;

        assert in.getType() == STRUCT;
        in.stepIn();
        {
            String file = null;
            ModuleIdentity id = null;

            while (in.next() != null)
            {
                String path = in.stringValue();

                switch (in.getFieldName())
                {
                    case "file":
                    {
                        file = path;
                        break;
                    }
                    case "module":
                    {
                        // TODO I'm too lazy to handle out-of-order fields.
                        assert file != null;
                        id = ModuleIdentity.forAbsolutePath(path);
                        break;
                    }
                    default:
                    {
                        // Ignore it.
                        break;
                    }
                }
            }

            if (id != null)
            {
                name = SourceName.forModule(id, new File(file));
            }
            else
            {
                name = SourceName.forFile(file);
            }
        }
        in.stepOut();

        return name;
    }


    private void readLocation(IonReader in, SourceName name)
        throws IOException
    {
        assert in.getType() == STRUCT;
        in.stepIn();
        {
            long    line    = 0;
            long    column  = 0;
            boolean covered = false;

            while (in.next() != null)
            {
                switch (in.getFieldName())
                {
                    case "line":
                    {
                        line = in.longValue();
                        break;
                    }
                    case "column":
                    {
                        column = in.longValue();
                        break;
                    }
                    case "covered":
                    {
                        covered = in.booleanValue();
                        break;
                    }
                    default:
                    {
                        // ignore it
                        break;
                    }
                }
            }

            SourceLocation loc =
                SourceLocation.forLineColumn(name, line, column);
            assert loc != null;

            if (coverableLocation(loc) && covered)
            {
                coverLocation(loc);
            }
        }
        in.stepOut();
    }


    private void readLocations(IonReader in, SourceName name)
        throws IOException
    {
        assert in.getType() == LIST;
        in.stepIn();
        {
            while (in.next() != null)
            {
                readLocation(in, name);
            }
        }
        in.stepOut();
    }


    private void readSource(IonReader in)
        throws IOException
    {
        assert in.getType() == STRUCT;
        in.stepIn();
        {
            SourceName name = null;

            while (in.next() != null)
            {
                switch (in.getFieldName())
                {
                    case "name":
                    {
                        name = readSourceName(in);
                        break;
                    }
                    case "locations":
                    {
                        // TODO I'm too lazy to handle out-of-order fields.
                        assert name != null;
                        readLocations(in, name);
                        break;
                    }
                    default:
                    {
                        // Ignore it.
                        break;
                    }
                }
            }
        }
        in.stepOut();
    }


    private void readFrom(IonReader in)
        throws IOException
    {
        while (in.next() != null)
        {
            readSource(in);
        }
    }


    private void readFrom(File where)
        throws IOException
    {
        IonSystem system = IonSystemBuilder.standard().build();
        try (InputStream is = new FileInputStream(where))
        {
            try (IonReader ir = system.newReader(is))
            {
                readFrom(ir);
            }
        }
    }
}
