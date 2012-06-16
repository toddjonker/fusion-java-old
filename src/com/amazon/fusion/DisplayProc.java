// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;
import java.io.OutputStreamWriter;

/**
 *
 */
final class DisplayProc
    extends Procedure
{
    DisplayProc()
    {
        //    "                                                                               |
        super("Outputs the VALUEs to stdout in human-readable form.\n" +
              "The result is not necessarily Ion data.",
              "value", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        try
        {
            OutputStreamWriter out = new OutputStreamWriter(System.out);
            try
            {
                for (Object arg : args)
                {
                    FusionValue.display(out, arg);
                }
            }
            finally
            {
                out.flush();
            }
        }
        catch (IOException e)
        {
            throw new FusionException(e);
        }

        return UNDEF;
    }
}
