// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

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
        return super.getMessage();
    }

    @Override
    public String getMessage()
    {
        StringBuilder out = new StringBuilder();
        out.append("Assertion failure: ");

        String superMessage = super.getMessage();
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
        FusionValue.write(out, myExpr);
        out.append("\nResult: ");
        FusionValue.write(out, myResult);

        return out.toString();
    }
}
