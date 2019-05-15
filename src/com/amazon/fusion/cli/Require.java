// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.FusionException;
import com.amazon.fusion.TopLevel;


class Require
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "Requires a single Fusion module.";
    private static final String HELP_USAGE =
        "require MODULE_NAME";
    private static final String HELP_BODY =
        "Requires a single Fusion module. Chain commands to require multiple modules\n" +
        "By itself, Fusion produces effect to output, but opens up modules for subsequent commands\n" +
        "Ex: ` fusion require /fusion/struct ';' eval '(begin (define s (mutable_struct \"a\" 1)) (put_m s \"modified\" true) s)' ` ==> {a:1,modified:true}\n" +
        "Be aware that regular shell quoting/escaping rules take place first.";


    Require()
    {
        super("require");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    @Override
    Executor makeExecutor(GlobalOptions globals, String[] args)
        throws UsageException
    {
        if (args.length != 1) throw usage();

        String moduleName = args[0];
        if (moduleName.length() == 0) throw usage();

        return new Executor(globals, moduleName);
    }


    static class Executor
        extends FusionExecutor
    {
        final String myModulePath;


        Executor(GlobalOptions globals, String modulePath)
        {
            super(globals);

            myModulePath = modulePath;
        }


        @Override
        public int execute()
            throws Exception
        {
            try
            {
                TopLevel top = runtime().getDefaultTopLevel();

                top.requireModule(myModulePath);
            }
            catch (FusionException e)
            {
                // TODO optionally display the stack trace
                System.err.println(e.getMessage());
                return 1;
            }

            return 0;
        }
    }
}
