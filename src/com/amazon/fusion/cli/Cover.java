// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion._Private_CoverageCollectorImpl;
import com.amazon.fusion._Private_CoverageWriter;
import java.io.File;

/**
 *
 */
class Cover
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "EXPERIMENTAL code coverage tool.";
    private static final String HELP_USAGE =
        "cover COVERAGE_DIR";
    private static final String HELP_BODY =
        "Loads Fusion code-coverage data from the given directory, then writes an HTML\n" +
        "report to index.html in the same directory.";


    Cover()
    {
        super("cover");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    @Override
    Executor makeExecutor(String[] args)
    {
        if (args.length != 1) return null;

        String dataDir = args[0];
        if (dataDir.length() == 0) return null;

        return new Executor(dataDir);
    }


    static class Executor
        implements Command.Executor
    {
        private final File myDataDir;

        private Executor(String fileName)
        {
            myDataDir = new File(fileName);
        }

        @Override
        public int execute()
            throws Exception
        {
            if (! myDataDir.isDirectory())
            {
                throw new IllegalArgumentException("Bad data dir");
            }

            _Private_CoverageCollectorImpl collector =
                _Private_CoverageCollectorImpl.fromDirectory(myDataDir);

            _Private_CoverageWriter renderer =
                new _Private_CoverageWriter(collector, null);

            File html = new File(myDataDir, "index.html");
            renderer.renderMarkedUpSource(html);

            System.out.print("Wrote Fusion coverage report to ");
            System.out.println(html.getPath());

            return 0;
        }
    }
}
