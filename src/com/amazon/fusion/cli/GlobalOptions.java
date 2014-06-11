// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionRuntimeBuilder;
import java.io.File;
import java.util.ArrayList;
import java.util.StringTokenizer;

/**
 *
 */
class GlobalOptions
{
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
