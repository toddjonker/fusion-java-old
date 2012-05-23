// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion.cli;

import com.amazon.fusion.ExitException;
import com.amazon.fusion.FusionException;
import com.amazon.fusion.FusionRuntime;
import com.amazon.fusion.FusionRuntimeBuilder;
import com.amazon.fusion.FusionValue;
import com.amazon.ion.IonException;
import java.io.Console;
import java.io.IOException;
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


    private final Console     myConsole;
    private final PrintWriter myOut;
    private final FusionRuntime myRuntime = FusionRuntimeBuilder.standard().build();


    Repl()
    {
        super("repl");
        putHelpText(HELP_ONE_LINER, HELP_USAGE, HELP_BODY);

        myConsole = System.console();
        myOut = (myConsole == null ? null : myConsole.writer());
    }


    //=========================================================================
    // Command Processing Methods

    @Override
    boolean processArguments(String[] args)
    {
        return (args.length == 0) && (myConsole != null);
    }


    @Override
    void execute()
    {
        welcome();
        while (rep())
        {
            // loop!
        }
    }


    private void welcome()
    {
        myOut.println("\n\033[1;31mWelcome to Fusion!\033[m\n");
        myOut.println("Type...");
        myOut.println("  (exit)            to exit");
        myOut.println("  (list_bindings)   to see available forms");
        myOut.println("  (help SOMETHING)  to see documentation; try '(help help)'!\n");
    }


    private boolean rep()
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
            FusionValue result = myRuntime.eval(line);
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

    private void print(FusionValue v)
    {
        if (v == FusionValue.UNDEF) return;

        try
        {
            v.write(myOut);
            myOut.println();
        }
        catch (IOException e)
        {
            // This shouldn't happen printing to a PrintWriter,
            // which doesn't throw exceptions.
            throw new IllegalStateException(e);
        }
    }
}
