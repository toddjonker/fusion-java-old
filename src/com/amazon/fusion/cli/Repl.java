// Copyright (c) 2012-2018 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion.FusionIo.write;
import static com.amazon.fusion.FusionVoid.isVoid;
import com.amazon.fusion.ExitException;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion.TopLevel;
import com.amazon.fusion._Private_Trampoline;
import com.amazon.ion.IonException;
import java.io.Console;
import java.io.PrintWriter;

/**
 * A simple Read-Eval-Print Loop for Fusion.
 */
class Repl
    extends Command
{
    //=+===============================================================================
    private static final String HELP_ONE_LINER =
        "Enter the interactive Read-Eval-Print Loop.";
    private static final String HELP_USAGE =
        "repl";
    private static final String HELP_BODY =
        "Enter the interactive console. Cannot be used when stdin or stdout have been\n" +
        "redirected.";


    Repl()
    {
        super("repl");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);
    }


    //=========================================================================

    @Override
    Executor makeExecutor(GlobalOptions globals,
                          Object        options,
                          String[]      args)
        throws UsageException
    {
        if (args.length != 0)
        {
            return null;  // Evokes a general usage exception
        }

        Console console = System.console();
        if (console == null)
        {
            throw new UsageException("There is no console available for the REPL.");
        }

        return new Executor(globals, console);
    }


    private static class Executor
        extends FusionExecutor
    {
        private       TopLevel      myTopLevel;
        private final Console       myConsole;
        private final PrintWriter   myOut;


        Executor(GlobalOptions globals, Console console)
        {
            super(globals);

            myConsole = console;
            myOut     = console.writer();
        }


        @Override
        FusionRuntimeBuilder runtimeBuilder()
            throws UsageException
        {
            FusionRuntimeBuilder builder = super.runtimeBuilder();
            _Private_Trampoline.setDocumenting(builder, true);
            return builder;
        }

        @Override
        public int execute()
            throws FusionException, UsageException
        {
            // Bootstrap the runtime before printing the welcome banner, so
            // that we don't do that when there's usage problems.
            myTopLevel = runtime().getDefaultTopLevel();
            myTopLevel.requireModule("/fusion/private/repl");

            welcome();

            while (rep())
            {
                // loop!
            }

            return 0;
        }


        private void welcome()
        {
            red("\nWelcome to Fusion!\n\n");
            myOut.println("Type...");
            myOut.println("  (exit)            to exit");
            myOut.println("  (help SOMETHING)  to see documentation; try '(help help)'!\n");
        }


        private boolean rep()
        {
            blue("$");
            String line = myConsole.readLine(" ");

            if (line == null)
            {
                // Print a newline otherwise the user's shell prompt will be on
                // the same line, and that's ugly.
                myOut.println();
                return false;
            }

            try
            {
                Object result = myTopLevel.eval(line);
                print(result);
            }
            catch (ExitException e)
            {
                blue("Goodbye!\n");
                myOut.flush();
                return false;
            }
            catch (FusionException | IonException e)
            {
                red(e.getMessage());
                myOut.println();
            }

            return true;
        }


        private void print(Object v)
            throws FusionException
        {
            if (v instanceof Object[])
            {
                Object[] results = (Object[]) v;
                for (Object r : results)
                {
                    write(myTopLevel, r, myOut);
                    myOut.println();
                }
            }
            else if (v != null && ! isVoid(myTopLevel, v))
            {
                write(myTopLevel, v, myOut);
                myOut.println();
            }
        }



        private void blue(String text)
        {
            myOut.print("\033[1;34m");
            myOut.print(text);
            myOut.print("\033[m");
        }

        private void red(String text)
        {
            myOut.print("\033[1;31m");
            myOut.print(text);
            myOut.print("\033[m");
        }
    }
}

