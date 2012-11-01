// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;


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
                // TODO support for non FusionValue?
                if (arg instanceof FusionValue)
                {
                    out.append('\n');

                    FeatureDocumentation doc = ((FusionValue) arg).document();

                    if (doc == null)
                    {
                        out.append("No documentation available.\n");
                    }
                    else
                    {
                        out.append('[');
                        // Using enum toString() allows display name to be changed
                        out.append(doc.myKind.toString());
                        out.append("]  ");
                        out.append(doc.myUsage);
                        out.append('\n');

                        if (doc.myBody != null)
                        {
                            out.append('\n');
                            out.append(doc.myBody);
                            out.append('\n');
                        }
                    }
                }
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
