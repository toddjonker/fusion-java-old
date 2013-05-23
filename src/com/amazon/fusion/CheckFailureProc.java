// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeDisplayManyToString;
import java.io.IOException;

/**
 * Fusion procedure to raise a unit test failure.
 */
final class CheckFailureProc
    extends Procedure
{
    CheckFailureProc()
    {
        //    "                                                                               |
        super("Raises an exception located at the given STX syntax. The MESSAGEs are\n" +
              "displayed as part of the error.",
              "stx", "message", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        SyntaxValue stx = checkSyntaxArg(0, args);

        String message = safeDisplayManyToString(eval, args, 1);

        throw new CheckFailure(stx, message);
    }


    @SuppressWarnings("serial")
    private class CheckFailure
        extends FusionException
    {
        private final SyntaxValue mySource;

        private CheckFailure(SyntaxValue source, String message)
        {
            super(message);
            mySource = source;
        }

        @Override
        public void displayMessage(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("Check failure: ");
            out.append(getBaseMessage());

            SourceLocation loc = mySource.getLocation();
            if (loc != null)
            {
                out.append("\nat ");
                loc.display(out);
            }

            out.append("\nSource: ");
            mySource.write(eval, out);
        }
    }
}
