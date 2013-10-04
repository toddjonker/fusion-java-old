// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion._Private_ModuleDocumenter.writeHtmlTree;
import java.io.File;


class Document
    extends Command
{
    //=+===============================================================================
    @SuppressWarnings("unused")
    private static final String HELP_ONE_LINER =
        "Generate reference documentation for a repository.";

    @SuppressWarnings("unused")
    private static final String HELP_USAGE =
        "document OUTPUT_DIR REPO_DIR";

    @SuppressWarnings("unused")
    private static final String HELP_BODY =
        "Given a REPO_DIR directory containing Fusion source code, generate reference\n" +
        "documentation (in HTML format) into the OUTPUT_DIR.";


    //=========================================================================
    // Constructors

    Document()
    {
        super("document");

        // We don't want this documented yet since its not stable.
//        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
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
        extends FusionExecutor
    {
        private final File myOutputDir;
        private final File myRepoDir;

        private Executor(File outputDir, File repoDir)
        {
            super(/* documenting */ true);

            myOutputDir = outputDir;
            myRepoDir   = repoDir;
        }


        @Override
        public int execute()
            throws Exception
        {
            writeHtmlTree(runtime(), myOutputDir, myRepoDir);
            return 0;
        }
    }
}
