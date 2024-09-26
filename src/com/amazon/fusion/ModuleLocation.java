// Copyright (c) 2013-2024 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;

/**
 * Abstract location of module source code.
 * <p>
 * These are produced by a {@link ModuleRepository} upon module discovery,
 * encapsulating the physical location of the module code.  They are consumed by
 * the {@link LoadHandler}, when loading the module.
 * <p>
 * "ModuleResource" might be a better name, or perhaps "ModuleCodeProvider"
 * since they can only be consumed once (see {@link #read}).
 * <p>
 * TODO: {@link SourceName} should track the repository holding the module.
 *    This would enable better messaging, cover Jar-based resources nicely, and
 *    allow the coverage report to partition source-files by repo root.
 */
abstract class ModuleLocation
{
    private final SourceName myName;

    private ModuleLocation(SourceName name)
    {
        this.myName = name;
    }


    /**
     * @return may be null.
     */
    final SourceName sourceName()
    {
        return myName;
    }


    /**
     * This method may only be called once per instance.
     * The result may be positioned on the value to be read.
     */
    abstract IonReader read(Evaluator eval)
        throws IOException;


    String parentDirectory()
    {
        return null;
    }


    @Override
    public String toString()
    {
        SourceName name = sourceName();
        return (name == null ? super.toString() : name.toString());
    }

    static ModuleLocation forIonReader(IonReader source, SourceName name)
    {
        return new IonReaderModuleLocation(source, name);
    }

    static ModuleLocation forFile(ModuleIdentity id, File sourceFile)
    {
        return new FileModuleLocation(id, sourceFile);
    }


    private static final class IonReaderModuleLocation
        extends ModuleLocation
    {
        private final IonReader  mySource;

        IonReaderModuleLocation(IonReader source, SourceName name)
        {
            super(name);
            mySource = source;
        }

        @Override
        IonReader read(Evaluator eval)
        {
            return mySource;
        }
    }


    abstract static class InputStreamModuleLocation
        extends ModuleLocation
    {
        InputStreamModuleLocation(SourceName name)
        {
            super(name);
        }

        abstract InputStream open()
            throws IOException;

        @Override
        IonReader read(Evaluator eval)
            throws IOException
        {
            IonReader reader = null;
            InputStream in = open();
            try
            {
                reader = eval.getIonReaderBuilder().build(in);
                return reader;
            }
            finally
            {
                if (reader == null)
                {
                    // We failed constructing the IonReader!
                    in.close();
                }
            }
        }
    }


    private static final class FileModuleLocation
        extends InputStreamModuleLocation
    {
        public FileModuleLocation(ModuleIdentity id, File sourceFile)
        {
            super(SourceName.forModule(id, sourceFile));
        }

        @Override
        InputStream open()
            throws IOException
        {
            return Files.newInputStream(sourceName().getFile().toPath());
        }

        @Override
        String parentDirectory()
        {
            return sourceName().getFile().getParentFile().getAbsolutePath();
        }
    }
}
