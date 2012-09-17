// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 * The {@code help} built-in procedure.
 * <p>
 * TODO add a Documentation model and move this to user space.
 */
final class HelpProc
    extends Procedure
{
    private static final class HelpDocument
        extends FusionValue
    {
        private final Object[] myArgs;

        private HelpDocument(Object[] args)
        {
            myArgs = args;
        }

        @Override
        public void write(Appendable out)
            throws IOException
        {
            for (Object arg : myArgs)
            {
                out.append('\n');

                // TODO FUSION-41 support displayHelp for non FusionValue?
                ((FusionValue)arg).displayHelp(out);
            }
        }
    }


    HelpProc()
    {
        //    "                                                                               |
        super("Prints the documentation of the given values, if available.",
              "value", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, final Object[] args)
    {
        // TODO write directly to current_output_port
        return new HelpDocument(args);
    }
}
