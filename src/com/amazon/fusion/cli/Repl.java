// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import static com.amazon.fusion.FusionVoid.isVoid;
import static com.amazon.fusion._Private_ModuleDocumenter.standardDocumentingRuntime;
import com.amazon.fusion.ExitException;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionValue;
import com.amazon.fusion._Private_HelpForm;
import com.amazon.fusion.TopLevel;
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
        implements Command.Executor
    {
        private final Console       myConsole;
        private final PrintWriter   myOut;
        private final FusionRuntime myRuntime;


        Executor(Console console)
        {
            myConsole = console;
            myOut     = console.writer();
            myRuntime = standardDocumentingRuntime();
        }


        @Override
        public int execute()
            throws FusionException
        {
            welcome();

            TopLevel top = myRuntime.getDefaultTopLevel();

            top.define("help", new _Private_HelpForm());

            while (rep(top))
            {
                // loop!
            }

            return 0;
        }


        private void welcome()
        {
            myOut.println("\n\033[1;31mWelcome to Fusion!\033[m\n");
            myOut.println("Type...");
            myOut.println("  (exit)            to exit");
            myOut.println("  (help SOMETHING)  to see documentation; try '(help help)'!\n");
        }


        private boolean rep(TopLevel top)
        {
            String line = myConsole.readLine("$ ");

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
                myOut.println("// Goodbye!");
                return false;
            }
            catch (FusionException e)
            {
                myOut.print("// ");
                myOut.println(e.getMessage());
            }
            catch (IonException e)
            {
                myOut.print("// ");
                myOut.println(e.getMessage());
            }

            return true;
        }


        private void print(Object v)
            throws FusionException
        {
            if (isVoid(v)) return;

            FusionValue.write(myOut, v);
            myOut.println();
        }
    }
}

