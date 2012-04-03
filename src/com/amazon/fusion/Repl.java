// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.Language.ExitException;
import com.amazon.ion.IonException;
import java.io.Console;
import java.io.IOException;
import java.io.PrintWriter;

/**
 * A simple Read-Eval-Print Loop for Fusion.
 */
final class Repl
{
    public static void main(String[] args)
    {
        Repl r = new Repl();
        r.welcome();
        r.loop();
    }

    private final Console myConsole = System.console();
    private final Language myLanguage = new Language();

    Repl()
    {
        if (myConsole == null)
        {
            throw new RuntimeException("No console available");
        }
    }

    void welcome()
    {
        PrintWriter writer = myConsole.writer();
        writer.println("\n\033[1;31mWelcome to Fusion!\033[m\n");
        writer.println("Type...");
        writer.println("  (exit)            to exit");
        writer.println("  (list_bindings)   to see available forms");
        writer.println("  (doc SOMETHING)   to see documentation; try '(doc doc)'!\n");
    }


    void loop()
    {
        while (rep())
        {
            // loop!
        }
    }

    private boolean rep()
    {
        PrintWriter writer = myConsole.writer();
        String line = myConsole.readLine("$ ");

        if (line == null)
        {
            // Print a newline otherwise the user's shell prompt will be on
            // the same line, and that's ugly.
            writer.println();
            return false;
        }

        try
        {
            FusionValue result = myLanguage.eval(line);
            try
            {
                myLanguage.write(result, writer);
            }
            catch (IOException e)
            {
                // This shouldn't happen printing to a PrintWriter,
                // which doesn't throw exceptions.
                throw new IllegalStateException(e);
            }
        }
        catch (ExitException e)
        {
            writer.println("// Goodbye!");
            return false;
        }
        catch (IonException e)
        {
            writer.print("// ");
            writer.println(e.getMessage());
        }

        return true;
    }
}
