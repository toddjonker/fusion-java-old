// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion._Private_CoverageCollectorImpl;
import com.amazon.fusion._Private_CoverageWriter;
import com.amazon.fusion._Private_Trampoline;
import java.io.File;

/**
 *
 */
class Cover
    extends Load
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "EXPERIMENTAL code coverage tool.";
    private static final String HELP_USAGE =
        "cover FILE";
    private static final String HELP_BODY =
        "Loads and evaluates the Fusion script in the given FILE, then writes a code\n" +
        "coverage report to coverage.html in the current directory.";


    Cover()
    {
        super("cover");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    @Override
    Executor processArguments(String[] args)
    {
        if (args.length != 1) return null;

        String fileName = args[0];
        if (fileName.length() == 0) return null;

        return new Executor(fileName);
    }


    static class Executor
        extends Load.Executor
    {
        _Private_CoverageCollectorImpl myCollector =
            new _Private_CoverageCollectorImpl();

        private Executor(String fileName)
        {
            super(fileName);
        }

        @Override
        FusionRuntimeBuilder runtimeBuilder()
        {
            FusionRuntimeBuilder builder = super.runtimeBuilder();
            _Private_Trampoline.setCoverageCollector(builder, myCollector);
            return builder;
        }


        @Override
        public int execute()
            throws Exception
        {
            int result = super.execute();

            _Private_CoverageWriter renderer =
                new _Private_CoverageWriter(myCollector, new File(myFileName));

            renderer.renderMarkedUpSource();

            return result;
        }
    }
}
