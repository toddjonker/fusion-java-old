// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion.FusionRuntimeBuilder.PROPERTY_BOOTSTRAP_REPOSITORY;
import com.amazon.fusion.FusionRuntimeBuilder;
import java.io.File;
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
        ;


    private String          myBootstrapPath;
    private ArrayList<File> myRepositories;


    public void setBootstrapRepository(String path)
    {
        myBootstrapPath = path;
    }


    public void setRepositories(String pathList)
    {
        if (myRepositories == null) myRepositories = new ArrayList<>();

        StringTokenizer tokenizer =
            new StringTokenizer(pathList, File.pathSeparator);

        while (tokenizer.hasMoreTokens())
        {
            String token = tokenizer.nextToken().trim();

            if (token.length() != 0)
            {
                File file = new File(token);
                myRepositories.add(file);
            }
        }
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

        return builder;
    }
}
