// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import com.amazon.ion.system.IonSystemBuilder;
import java.io.Console;
import java.io.IOException;
import java.io.PrintWriter;


class Repl
{
    public static void main(String[] args)
    {
        Repl r = new Repl();
        r.loop();
    }

    private final Console myConsole = System.console();
    private final IonSystem mySystem = IonSystemBuilder.standard().build();
    private final Environment myEnvironment = new CoreEnvironment(mySystem);
    private final Evaluator myEvaluator = new Evaluator();

    Repl()
    {
        if (myConsole == null)
        {
            throw new RuntimeException("No console available");
        }
    }


    void loop()
    {
        myConsole.printf("Welcome to Fusion!\n");
        myConsole.printf("Type 'exit' to exit.\n");
        myConsole.printf("Type '(list_bindings)' to see available forms.\n");

        while (rep())
        {
            // loop!
        }
    }

    private boolean rep()
    {
        String line = myConsole.readLine("$ ");

        if (line == null)
        {
            // Print a newline otherwise the user's shell prompt will be on
            // the same line, and that's ugly.
            myConsole.writer().println();
            return false;
        }

        boolean cont = ! line.equals("exit");
        if (cont)
        {
            PrintWriter writer = myConsole.writer();

            try
            {
                FusionValue result = eval(line);
                if (result != null)
                {
                    try
                    {
                        result.print(myConsole.writer());
                    }
                    catch (IOException e)
                    {
                        // This shouldn't happen since the Console provides a
                        // PrintWriter, which doesn't throw exceptions.
                        throw new IllegalStateException(e);
                    }
                }
                else
                {
                    writer.println("// No value");
                }
            }
            catch (IonException e)
            {
                writer.print("// ");
                writer.println(e.getMessage());
            }
        }

        return cont;
    }


    private FusionValue eval(String line)
    {
        IonValue expression = mySystem.singleValue(line);
        expression.makeReadOnly();
        return myEvaluator.eval(myEnvironment, expression);
    }
}
