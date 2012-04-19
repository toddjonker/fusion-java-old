// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonException;
import com.amazon.ion.IonSexp;
import java.io.IOException;
import java.io.Writer;

/**
 * The {@code help} built-in function.
 * <p>
 * TODO add a Documentation model and move this to user space.
 */
final class HelpFunction
    extends FunctionValue
{
    HelpFunction()
    {
        //    "                                                                               |
        super("Prints the documentation of the given values, if available.",
              "value", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
    {
        FusionValue result = new FusionValue()
        {
            @Override
            void display(Writer out)
                throws IOException
            {
                for (FusionValue arg : args)
                {
                    out.write('\n');
                    arg.printHelp(out);
                }
            }

            @Override
            FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
            {
                throw new IonException("not invokable");
            }
        };

        return result;
    }
}
