// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;

/**
 *
 */
@SuppressWarnings("serial")
final class FusionAssertionFailure
    extends FusionException
{
    private final IonValue myExpr;
    private final FusionValue myResult;

    FusionAssertionFailure(String message, IonValue expr, FusionValue result)
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
        String message =
            "Assertion failure: " + super.getMessage() +
            "\nExpression: " + myExpr +
            "\nResult: " + myResult.write();
        return message;
    }
}
