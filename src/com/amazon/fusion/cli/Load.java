// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.TopLevel;
import java.io.File;


class Load
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "Load and evaluate a script.";
    private static final String HELP_USAGE =
        "load FILE";
    private static final String HELP_BODY =
        "Loads and evaluates the Fusion script in the given FILE.  If the result of the\n" +
        "last expression is not void, it is sent to standard output via `write`.";


    Load()
    {
        super("load");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    @Override
    Executor makeExecutor(GlobalOptions globals, String[] args)
        throws UsageException
    {
        if (args.length != 1) throw usage();

        String fileName = args[0];
        if (fileName.length() == 0) throw usage();

        File file = new File(fileName);
        if (! (file.canRead() && file.isFile()))
        {
            throw usage("Not a readable file: " + fileName);
        }

        return new Executor(globals, file);
    }


    static class Executor
        extends FusionExecutor
    {
        final File myFile;


        Executor(GlobalOptions globals, File file)
        {
            super(globals);

            myFile = file;
        }


        @Override
        public Object execute(TopLevel top)
            throws Exception
        {
            return top.load(myFile);
        }
    }
}
