// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

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
        "Generate a code coverage report.";
    private static final String HELP_USAGE =
        "report_coverage COVERAGE_DATA_DIR REPORT_DIR";
    private static final String HELP_BODY =
        "Reads Fusion code-coverage data from the COVERAGE_DATA_DIR, then writes an\n" +
        "HTML report to the REPORT_DIR.";


    Cover()
    {
        super("report_coverage");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    @Override
    Executor makeExecutor(String[] args)
        throws UsageException
    {
        if (args.length != 2) return null;

        String dataPath = args[0];
        if (dataPath.isEmpty()) return null;

        File dataDir = new File(dataPath);
        if (! dataDir.isDirectory())
        {
            String message =
                "Coverage data directory isn't a directory: " + dataPath;
            throw new UsageException(this, message);
        }

        String reportPath = args[1];
        if (reportPath.isEmpty()) return null;

        File reportDir = new File(reportPath);
        if (reportDir.exists() && ! reportDir.isDirectory())
        {
            String message =
                "Report directory isn't a directory: " + reportPath;
            throw new UsageException(this, message);
        }

        return new Executor(dataDir, reportDir);
    }


    static class Executor
        implements Command.Executor
    {
        private final File myDataDir;
        private final File myReportDir;

        private Executor(File dataDir, File reportDir)
        {
            myDataDir   = dataDir;
            myReportDir = reportDir;
        }

        @Override
        public int execute()
            throws Exception
        {
            _Private_CoverageWriter renderer =
                new _Private_CoverageWriter(myDataDir);

            renderer.renderFullReport(myReportDir);

            System.out.print("Wrote Fusion coverage report to ");
            System.out.println(myReportDir.getPath());

            return 0;
        }
    }
}
