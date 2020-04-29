// Copyright (c) 2014-2020 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.File;
import java.io.IOException;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
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
 *
 * @see CoverageConfiguration
 */
public final class _Private_CoverageCollectorImpl
    implements _Private_CoverageCollector
{
    private final CoverageConfiguration myConfig;

    /** Where we store our metrics. */
    private final CoverageDatabase myDatabase;


    private _Private_CoverageCollectorImpl(File dataDir)
        throws FusionException, IOException
    {
        myConfig   = new CoverageConfiguration(dataDir);
        myDatabase = new CoverageDatabase(dataDir);
    }



    private static final
    ReferenceQueue<_Private_CoverageCollectorImpl> ourReferenceQueue =
        new ReferenceQueue<>();


    private static final class CollectorRef
        extends WeakReference<_Private_CoverageCollectorImpl>
    {
        private File             myFile;
        private CoverageDatabase myDatabase;

        CollectorRef(_Private_CoverageCollectorImpl referent)
        {
            super(referent, ourReferenceQueue);

            myDatabase = referent.myDatabase;
        }

        void flushDatabase()
            throws IOException
        {
            try
            {
                synchronized (this)
                {
                    if (myDatabase != null)
                    {
                        // If we fail to write the database, don't try it again.
                        CoverageDatabase db = myDatabase;
                        myDatabase = null;
                        db.write();
                    }
                }
            }
            finally
            {
                removeFromCache(myFile, this);
            }
        }
    }


    private static class Flusher
        implements Runnable
    {
        @Override
        public void run()
        {
            try
            {
                while (true)
                {
                    try
                    {
                        CollectorRef ref =
                            (CollectorRef) ourReferenceQueue.remove();
                        ref.flushDatabase();
                    }
                    catch (InterruptedException e)
                    {
                        break;
                    }
                    catch (IOException e)
                    {
                        // TODO Auto-generated catch block
                        e.printStackTrace();
                    }
                }
            }
            finally
            {
                synchronized (Flusher.class)
                {
                    ourFlusherThread = null;
                }
            }
        }


        private static Thread ourFlusherThread = null;

        static synchronized void start()
        {
            if (ourFlusherThread == null)
            {
                ourFlusherThread =
                    new Thread(new Flusher(),
                               "Fusion coverage metrics flusher");
                ourFlusherThread.setDaemon(true);
                ourFlusherThread.start();
            }
        }

        /** Called when there are no more references to flush. */
        static synchronized void stop()
        {
            if (ourFlusherThread != null)
            {
                ourFlusherThread.interrupt();
                ourFlusherThread = null;
            }
        }
    }



    private static Map<File,CollectorRef> ourCollectorCache;

    private static Thread ourShutdownHook;
    private static boolean ourShutdownHasStarted;


    /**
     * Called before every PUT into the cache.
     */
    private static synchronized void initCache()
        throws FusionException
    {
        if (ourCollectorCache == null)
        {
            ourCollectorCache = new HashMap<>();
        }

        if (ourShutdownHook == null)
        {
            ourShutdownHook = new Thread()
            {
                @Override
                public void run()
                {
                    emptyCache();
                }
            };

            try
            {
                Runtime.getRuntime().addShutdownHook(ourShutdownHook);
            }
            catch (IllegalStateException e)
            {
                throw new FusionException("The JMV is shutting down.");
            }
        }
        else if (ourShutdownHasStarted)
        {
            throw new FusionException("The JMV is shutting down.");
        }
    }


    private static synchronized void addToCache(File file,  CollectorRef ref)
    {
        ourCollectorCache.put(file, ref);

        Flusher.start();
    }


    /**
     * Remove an entry from the cache AFTER it has been flushed.
     */
    private static synchronized void removeFromCache(File file,
                                                     CollectorRef ref)
    {
        assert ref.get() == null;

        CollectorRef current = ourCollectorCache.get(file);
        if (current == ref)
        {
            ourCollectorCache.remove(file);
        }

        if (ourCollectorCache.isEmpty())
        {
            assert ourShutdownHook != null;
            try
            {
                Runtime.getRuntime().removeShutdownHook(ourShutdownHook);
            }
            catch (IllegalStateException e)
            {
                // The JMV is shutting down. Nothing to do.
            }

            Flusher.stop();
        }
    }

    /**
     * Called by the JVM shutdown hook, to ensure everything gets flushed.
     */
    private static synchronized void emptyCache()
    {
        ourShutdownHasStarted = true;

        // Snapshot the values in the cache, to avoid concurrent
        // modification problems. If the cache isn't empty, Flusher thread is
        // still running.
        Set<CollectorRef> files = new HashSet<>(ourCollectorCache.values());
        for (CollectorRef ref : files)
        {
            if (ref != null)
            {
                ref.clear();
                try
                {
                    ref.flushDatabase();
                }
                catch (IOException e)
                {
                    String message = "Error writing coverage data";
                    throw new RuntimeException(message, e);
                }
            }
        }
    }

    public static synchronized
    _Private_CoverageCollectorImpl fromDirectory(File dataDir)
        throws FusionException
    {
        initCache();

        _Private_CoverageCollectorImpl collector;

        CollectorRef ref = ourCollectorCache.get(dataDir);
        if (ref != null)
        {
            collector = ref.get();
            if (collector != null) return collector;

            // The prior collector has been GCed, so write any remaining state.
            // We want this to happen BEFORE creating a new collector using
            // the same directory.
            try
            {
                ref.flushDatabase();
            }
            catch (IOException e)
            {
                throw new FusionException("Error writing coverage data", e);
            }
            // ref is now useless. Fall through and create a new one.
        }

        try
        {
            collector = new _Private_CoverageCollectorImpl(dataDir);
        }
        catch (IOException e)
        {
            throw new FusionException("Error reading coverage data", e);
        }

        addToCache(dataDir, new CollectorRef(collector));

        return collector;
    }

    public static _Private_CoverageCollectorImpl fromDirectory(String dataDir)
        throws FusionException
    {
        return fromDirectory(new File(dataDir));
    }


    //=========================================================================
    // Managing the coverage data


    void noteRepository(File repoDir)
    {
        myDatabase.noteRepository(repoDir);
    }


    @Override
    public boolean coverableLocation(SourceLocation loc)
    {
        boolean coverable = myConfig.locationIsSelected(loc);

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
