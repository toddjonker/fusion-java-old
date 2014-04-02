// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;
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
    private final Map<SourceLocation,Boolean> myLocations = new HashMap<>();

    @Override
    public synchronized boolean coverableLocation(SourceLocation loc)
    {
        myLocations.put(loc, Boolean.FALSE);

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
        return myLocations.get(loc);
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

}
