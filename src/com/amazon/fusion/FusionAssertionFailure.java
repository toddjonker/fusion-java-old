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
        String superMessage = super.getMessage();
        String message =
            "Assertion failure: " + (superMessage == null ? "" : superMessage) +
            "\nExpression: " + myExpr +
            "\nResult: " + FusionValue.writeToString(myResult);
        return message;
    }
}
