// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import java.io.IOException;

/**
 * The {@code help} built-in function.
 * <p>
 * TODO add a Documentation model and move this to user space.
 */
final class HelpFunction
    extends FunctionValue
{
    private static final class HelpDocument
        extends FusionValue
    {
        private final FusionValue[] myArgs;

        private HelpDocument(FusionValue[] args)
        {
            myArgs = args;
        }

        @Override
        public void write(Appendable out)
            throws IOException
        {
            for (FusionValue arg : myArgs)
            {
                out.append('\n');
                arg.displayHelp(out);
            }
        }

        @Override
        FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
            throws FusionException
        {
            throw new FusionException("not invokable");
        }
    }


    HelpFunction()
    {
        //    "                                                                               |
        super("Prints the documentation of the given values, if available.",
              "value", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
    {
        return new HelpDocument(args);
    }
}
