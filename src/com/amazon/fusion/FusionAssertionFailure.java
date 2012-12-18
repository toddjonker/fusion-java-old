// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionWrite.safeWrite;
import java.io.IOException;

@SuppressWarnings("serial")
final class FusionAssertionFailure
    extends FusionException
{
    private final SyntaxValue myExpr;
    private final Object      myResult;

    FusionAssertionFailure(String message, SyntaxValue expr, Object result)
    {
        super(message);
        myExpr = expr;
        myResult = result;
    }

    String getUserMessage()
    {
        return getBaseMessage();
    }

    @Override
    public void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        out.append("Assertion failure: ");

        String superMessage = getBaseMessage();
        if (superMessage != null)
        {
            out.append(superMessage);
        }

        SourceLocation location = myExpr.getLocation();
        if (location != null)
        {
            out.append("\nLocation: ");
            location.display(out);
        }

        out.append("\nExpression: ");
        safeWrite(eval, out, myExpr);
        out.append("\nResult: ");
        safeWrite(eval, out, myResult);
    }
}
