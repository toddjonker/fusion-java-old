// Copyright (c) 2012-2019 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion.FusionIo.write;
import static com.amazon.fusion.FusionVoid.isVoid;
import com.amazon.fusion.ExitException;
import com.amazon.fusion.FusionException;
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
    Executor makeExecutor(GlobalOptions globals,
                          Object        options,
                          String[]      args)
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
        public int execute()
            throws Exception
        {
            try
            {
                TopLevel top = runtime().getDefaultTopLevel();

                Object result = top.eval(myEvalText);

                if (result instanceof Object[])
                {
                    Object[] results = (Object[]) result;
                    for (Object r : results)
                    {
                        write(top, r, System.out);
                        System.out.println();
                    }
                }
                else if (result != null && ! isVoid(top, result))
                {
                    write(top, result, System.out);
                    System.out.println();
                }

                System.out.flush();
            }
            catch (ExitException e)
            {
                // Do nothing, just return successfully.
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
