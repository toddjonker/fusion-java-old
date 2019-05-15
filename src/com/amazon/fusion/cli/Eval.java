// Copyright (c) 2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.TopLevel;


class Eval
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "Evaluate an inline script.";
    private static final String HELP_USAGE =
        "eval EVAL_TEXT";
    private static final String HELP_BODY =
        "Evaluate an inline script from the given EVAL_TEXT. If the result of the\n" +
        "last expression is not void, it is sent to standard output via `write`.\n" +
        "Be aware that regular shell quoting/escaping rules take place first.";


    Eval()
    {
        super("eval");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    @Override
    Executor makeExecutor(GlobalOptions globals, String[] args)
        throws UsageException
    {
        if (args.length != 1) throw usage();

        String evalText = args[0];
        if (evalText.length() == 0) throw usage();

        return new Executor(globals, evalText);
    }


    static class Executor
        extends FusionExecutor
    {
        final String myEvalText;


        Executor(GlobalOptions globals, String evalText)
        {
            super(globals);

            myEvalText = evalText;
        }


        @Override
        public Object execute(TopLevel top)
            throws Exception
        {
            return top.eval(myEvalText);
        }
    }
}
