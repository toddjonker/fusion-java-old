// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion.FusionIo.write;
import static com.amazon.fusion.FusionVoid.isVoid;
import com.amazon.fusion.ExitException;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.TopLevel;
import com.amazon.fusion._Private_HelpForm;
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
    Executor processArguments(String[] args)
    {
        Console console = System.console();
        if ((args.length == 0) && (console != null))
        {
            return new Executor(console);
        }
        return null;
    }


    private static class Executor
        extends FusionExecutor
    {
        private final Console       myConsole;
        private final PrintWriter   myOut;


        Executor(Console console)
        {
            super(/* documenting */ true);

            myConsole = console;
            myOut     = console.writer();
        }


        @Override
        public int execute()
            throws FusionException
        {
            welcome();

            TopLevel top = runtime().getDefaultTopLevel();

            top.define("help", new _Private_HelpForm());

            while (rep(top))
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


        private boolean rep(TopLevel top)
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
                Object result = top.eval(line);
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
            TopLevel top = runtime().getDefaultTopLevel();

            if (v instanceof Object[])
            {
                Object[] results = (Object[]) v;
                for (Object r : results)
                {
                    write(top, r, myOut);
                    myOut.println();
                }
            }
            else if (v != null && ! isVoid(top, v))
            {
                write(top, v, myOut);
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

