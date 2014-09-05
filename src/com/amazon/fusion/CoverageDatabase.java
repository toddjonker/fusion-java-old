// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.ion.IonType.LIST;
import static com.amazon.ion.IonType.STRING;
import static com.amazon.ion.IonType.STRUCT;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonType;
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
import java.util.Set;


class CoverageDatabase
{

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


    private final File myCoverageFile;

    private final Set<File> myRepositories = new HashSet<>();

    private final Map<SourceLocation,Boolean> myLocations = new HashMap<>();


    public CoverageDatabase(File dataDir)
        throws IOException
    {
        myCoverageFile = new File(dataDir, "coverage.ion");

        if (myCoverageFile.exists())
        {
            read();
        }
    }


    synchronized void noteRepository(File repoDir)
    {
        myRepositories.add(repoDir);
    }


    synchronized Set<File> getRepositories()
    {
        return myRepositories;
    }


    synchronized void noteCoverableLocation(SourceLocation loc)
    {
        Boolean prev = myLocations.put(loc, Boolean.FALSE);

        // If already covered, don't un-cover it!
        if (prev != null && prev)
        {
            myLocations.put(loc, prev);
        }
    }


    synchronized void coverLocation(SourceLocation loc)
    {
        myLocations.put(loc, Boolean.TRUE);
    }


    /** Has a location been covered? */
    synchronized boolean locationCovered(SourceLocation loc)
    {
        Boolean covered = myLocations.get(loc);
        return (covered != null && covered);
    }


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


    /**
     * @return not null.
     */
    synchronized Set<SourceLocation> locations()
    {
        return myLocations.keySet();
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


    //=====================================================================


    private void writeRepositories(IonWriter iw)
        throws IOException
    {
        iw.stepIn(IonType.LIST);
        {
            for (File f : myRepositories)
            {
                String path = f.getAbsolutePath();
                iw.writeString(path);
            }
        }
        iw.stepOut();
    }


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


    synchronized void write()
        throws IOException
    {
        FusionUtils.createParentDirs(myCoverageFile);
        try (OutputStream out = new FileOutputStream(myCoverageFile))
        {
            try (IonWriter iw = IonTextWriterBuilder.minimal().build(out))
            {
                writeRepositories(iw);

                for (SourceName name : sourceNames())
                {
                    if (name.getFile() != null)
                    {
                        writeSource(iw, name);
                    }
                }
            }
        }
    }


    //=====================================================================


    private void readRepositories(IonReader in)
    {
         in.next();
         assert in.getType() == LIST;
         in.stepIn();
         {
             while (in.next() == STRING)
             {
                 String path = in.stringValue();
                 myRepositories.add(new File(path));
             }
         }
         in.stepOut();
    }


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

            noteCoverableLocation(loc);
            if (covered)
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


    private void read()
        throws IOException
    {
        IonSystem system = IonSystemBuilder.standard().build();
        try (InputStream is = new FileInputStream(myCoverageFile))
        {
            try (IonReader ir = system.newReader(is))
            {
                readRepositories(ir);

                while (ir.next() != null)
                {
                    readSource(ir);
                }
            }
        }
    }
}
