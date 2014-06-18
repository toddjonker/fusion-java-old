// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
import java.util.Set;

/**
 *
 */
public final class _Private_CoverageCollectorImpl
    implements _Private_CoverageCollector
{
    private final File myDataDir;
    private final File myCoverageFile;

    private final Map<SourceLocation,Boolean> myLocations = new HashMap<>();


    public _Private_CoverageCollectorImpl()
    {
        myDataDir = null;
        myCoverageFile = null;
    }

    private _Private_CoverageCollectorImpl(File dataDir)
        throws IOException
    {
        myDataDir = dataDir;
        myCoverageFile = new File(myDataDir, "coverage.ion");

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


    @Override
    public synchronized boolean coverableLocation(SourceLocation loc)
    {
        Boolean prev = myLocations.put(loc, Boolean.FALSE);

        // If already covered, don't un-cover it!
        if (prev != null)
        {
            myLocations.put(loc, prev);
        }

        return true; // Collect coverage for everything
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
            iw.setFieldName("file");
            iw.writeString(name.getFile().getPath());

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
                    case "file":
                    {
                        name = SourceName.forFile(in.stringValue());
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
