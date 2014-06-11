// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion._Private_ModuleDocumenter.writeHtmlTree;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion._Private_Trampoline;
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
    Executor makeExecutor(GlobalOptions globals,
                          Object        options,
                          String[]      args)
    {
        if (args.length != 2) return null;

        File outputDir = new File(args[0]);
        File repoDir   = new File(args[1]);

        if (outputDir.isFile())
        {
            System.err.print("Output location is a file: ");
            System.err.println(outputDir);
            return null;
        }

        if (! repoDir.isDirectory())
        {
            System.err.print("Repository is not a directory: ");
            System.err.println(repoDir);
            return null;
        }

        if (! new File(repoDir, "src").isDirectory())
        {
            System.err.print("Repository has no src directory: ");
            System.err.println(repoDir);
            return null;
        }

        return new Executor(globals, outputDir, repoDir);
    }


    private static class Executor
        extends FusionExecutor
    {
        private final File myOutputDir;
        private final File myRepoDir;

        private Executor(GlobalOptions globals, File outputDir, File repoDir)
        {
            super(globals);

            myOutputDir = outputDir;
            myRepoDir   = repoDir;
        }

        @Override
        FusionRuntimeBuilder runtimeBuilder()
            throws UsageException
        {
            FusionRuntimeBuilder builder = super.runtimeBuilder();
            builder.addRepositoryDirectory(myRepoDir);
            _Private_Trampoline.setDocumenting(builder, true);
            return builder;
        }

        @Override
        public int execute()
            throws Exception
        {
            File srcDir = new File(myRepoDir, "src");
            writeHtmlTree(runtime(), myOutputDir, srcDir);
            return 0;
        }
    }
}
