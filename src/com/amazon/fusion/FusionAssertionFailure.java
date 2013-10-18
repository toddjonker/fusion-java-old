// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWrite;
import java.io.IOException;

@SuppressWarnings("serial")
final class FusionAssertionFailure
    extends FusionException
{
    private final SourceLocation myLocation;
    private final String         myExpression;
    private final Object         myResult;

    /**
     * @param message may be null.
     * @param location may be null.
     * @param result must not be null.
     */
    FusionAssertionFailure(String message,
                           SourceLocation location,
                           String expression,
                           Object result)
    {
        super(message);
        myLocation = location;
        myExpression = expression;
        myResult = result;
    }

    String getUserMessage()
    {
        return getBaseMessage();
    }

    @Override
    void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        out.append("Assertion failure: ");

        String superMessage = getBaseMessage();
        if (superMessage != null)
        {
            out.append(superMessage);
        }

        if (myLocation != null)
        {
            out.append("\nLocation: ");
            myLocation.display(out);
        }

        out.append("\nExpression: ");
        out.append(myExpression);
        out.append("\nResult: ");
        safeWrite(eval, out, myResult);
    }
}
