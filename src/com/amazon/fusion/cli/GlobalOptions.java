// Copyright (c) 2014-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion.FusionRuntimeBuilder.PROPERTY_BOOTSTRAP_REPOSITORY;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.ion.IonException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 *
 */
class GlobalOptions
{
    static final String HELP =
        "\n\n"
        + "Global Options\n"
        + "==============\n"
        + "--bootstrapRepository DIR\n"
        + "  Path to a Fusion bootstrap repository. The JVM system property\n"
        + "  " + PROPERTY_BOOTSTRAP_REPOSITORY + " has the same effect.\n"
        + "--repositories DIR" + File.pathSeparator + "DIR...\n"
        + "  Additional user repositories. This option can be given more than once and\n"
        + "  all DIRs will be used.\n"
        + "--catalogs FILE" + File.pathSeparator + "FILE...\n"
        + "  Files containing Ion shared symbol tables. This option can be given more\n"
        + "  than once and all FILEs will be used."
        ;


    private String          myBootstrapPath;
    private ArrayList<File> myRepositories;
    private ArrayList<File> myCatalogs;


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

        return builder;
    }
}
