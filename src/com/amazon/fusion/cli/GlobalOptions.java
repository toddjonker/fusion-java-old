// Copyright (c) 2014-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion.FusionRuntimeBuilder.PROPERTY_BOOTSTRAP_REPOSITORY;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion._Private_Trampoline;
import com.amazon.ion.IonException;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 * Stores options and data shared by the entire execution sequence.
 */
final class GlobalOptions
{
    static final String HELP =
        "\n\n"
        + "Global Options\n"
        + "==============\n"
        + "\n"
        + "--bootstrapRepository DIR\n"
        + "\n"
        + "  Path to a Fusion bootstrap repository. The JVM system property\n"
        + "  " + PROPERTY_BOOTSTRAP_REPOSITORY + " has the same effect.\n"
        + "\n"
        + "--repositories DIR" + File.pathSeparator + "DIR...\n"
        + "\n"
        + "  Additional user repositories. This option can be given more than once and\n"
        + "  all DIRs will be used.\n"
        + "\n"
        + "--catalogs CATALOG" + File.pathSeparator + "CATALOG...\n"
        + "\n"
        + "  Sources of Ion shared symbol tables. When a CATALOG is a real file, it must\n"
        + "  contain only serialized shared symbol tables. When a CATALOG is a directory,\n"
        + "  it is traversed recursively. This option can be given more than once and all\n"
        + "  CATALOGs will be used.\n"
        ;


    private InputStream     myStdin;
    private PrintStream     myStdout;
    private PrintStream     myStderr;
    private String          myBootstrapPath;
    private ArrayList<File> myRepositories;
    private ArrayList<File> myCatalogs;
    private boolean         myDocsEnabled;
    private FusionRuntime   myRuntime;


    GlobalOptions(InputStream stdin, PrintStream stdout, PrintStream stderr)
    {
        myStdin  = stdin;
        myStdout = stdout;
        myStderr = stderr;
    }

    InputStream stdin()
    {
        return myStdin;
    }

    PrintStream stdout()
    {
        return myStdout;
    }

    PrintStream stderr()
    {
        return myStderr;
    }


    public void setBootstrapRepository(String path)
    {
        myBootstrapPath = path;
    }


    private static void addFiles(ArrayList<File> files, String pathList)
    {
        StringTokenizer tokenizer =
            new StringTokenizer(pathList, File.pathSeparator);

        while (tokenizer.hasMoreTokens())
        {
            String token = tokenizer.nextToken().trim();

            if (token.length() != 0)
            {
                File file = new File(token);
                files.add(file);
            }
        }
    }


    public void setRepositories(String pathList)
    {
        if (myRepositories == null) myRepositories = new ArrayList<>();

        addFiles(myRepositories, pathList);
    }


    public void setCatalogs(String pathList)
    {
        if (myCatalogs == null) myCatalogs = new ArrayList<>();

        addFiles(myCatalogs, pathList);
    }


    void collectDocumentation()
    {
        myDocsEnabled = true;
    }


    FusionRuntimeBuilder runtimeBuilder()
        throws UsageException
    {
        FusionRuntimeBuilder builder = FusionRuntimeBuilder.standard();

        if (myBootstrapPath != null)
        {
            File dir = new File(myBootstrapPath);

            try
            {
                builder.setBootstrapRepository(dir);
            }
            catch (IllegalArgumentException e)
            {
                String message = "bootstrapRepository: " + e.getMessage();
                throw new UsageException(message);
            }
        }

        if (myRepositories != null)
        {
            try
            {
                for (File repo : myRepositories)
                {
                    builder.addRepositoryDirectory(repo);
                }
            }
            catch (IllegalArgumentException e)
            {
                String message = "repositories: " + e.getMessage();
                throw new UsageException(message);
            }
        }

        if (myCatalogs != null)
        {
            try
            {
                IonCatalogLoader loader = new IonCatalogLoader();
                loader.loadFiles(myCatalogs);

                builder.withDefaultIonCatalog(loader.getCatalog());
            }
            catch (IOException | IonException e)
            {
                String message = "catalogs: " + e.getMessage();
                throw new UsageException(message);
            }
        }

        builder.setInitialCurrentOutputPort(myStdout);

        _Private_Trampoline.setDocumenting(builder, myDocsEnabled);

        return builder;
    }

    FusionRuntime runtime()
        throws FusionException, UsageException
    {
        // Lazily loading a runtime that can be global across commands.
        if (myRuntime == null)
        {
            FusionRuntimeBuilder builder = runtimeBuilder();
            myRuntime = builder.build();
        }
        return myRuntime;
    }
}
