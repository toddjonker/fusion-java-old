// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.safeDisplayManyToString;
import java.io.IOException;
import java.util.List;

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
              "stx_param", "message", DOTDOTDOT); // FIXME doc above
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);

        String message = safeDisplayManyToString(eval, args, 1);

        DynamicParameter param = (DynamicParameter) args[0];
        List<SyntaxValue> stack = param.allValues(eval);

        throw new CheckFailure(stack, message);
    }


    @SuppressWarnings("serial")
    private class CheckFailure
        extends FusionException
    {
        private final List<SyntaxValue> myStack;

        private CheckFailure(List<SyntaxValue> stack, String message)
        {
            super(message);
            myStack = stack;
        }

        @Override
        public void displayMessage(Evaluator eval, Appendable out)
            throws IOException, FusionException
        {
            out.append("Check failure: ");
            out.append(getBaseMessage());

            for (SyntaxValue mySource : myStack)
            {
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
}
