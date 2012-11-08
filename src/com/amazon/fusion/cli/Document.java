// Copyright (c) 2005-2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion._Private_ModuleDocumenter.writeHtmlTree;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import java.io.File;


class Document
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "Generate reference documentation for a repository.";

    private static final String HELP_USAGE =
        "document OUTPUT_DIR REPO_DIR";

    private static final String HELP_BODY =
        "Given a REPO_DIR directory containing Fusion source code, generate reference\n" +
        "documentation (in MarkDown format) into the OUTPUT_DIR.";


    //=========================================================================
    // Constructors

    Document()
    {
        super("document");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    //=========================================================================


    @Override
    Executor processArguments(String[] args)
    {
        if (args.length != 2) return null;

        File outputDir = new File(args[0]);
        File repoDir   = new File(args[1]);

        if (outputDir.isFile())
        {
            System.err.print("Output location is a file: ");
            System.err.println(args[0]);
            return null;
        }

        if (! repoDir.isDirectory())
        {
            System.err.print("Repository is not a directory: ");
            System.err.println(args[1]);
            return null;
        }

        return new Executor(outputDir, repoDir);
    }


    private static class Executor
        implements Command.Executor
    {
        private final FusionRuntime myRuntime;
        private final File myOutputDir;
        private final File myRepoDir;

        private Executor(File outputDir, File repoDir)
        {
            myRuntime   = FusionRuntimeBuilder.standard().build();
            myOutputDir = outputDir;
            myRepoDir   = repoDir;
        }


        @Override
        public int execute()
            throws Exception
        {
            writeHtmlTree(myRuntime, myOutputDir, myRepoDir);
            return 0;
        }
    }
}
