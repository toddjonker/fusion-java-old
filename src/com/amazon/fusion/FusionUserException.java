// Copyright (c) 2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 * Represents an arbitrary, non-exception Fusion value thrown by {@code raise}.
 */
@SuppressWarnings("serial")
final class FusionUserException
    extends FusionException
{
    private final Object myRaisedValue;

    FusionUserException(String message, Object raisedValue)
    {
        super(message);
        myRaisedValue = raisedValue;
    }

    FusionUserException(String message, Throwable cause, Object raisedValue)
    {
        super(message, cause);
        myRaisedValue = raisedValue;
    }

    FusionUserException(Throwable cause, Object raisedValue)
    {
        super(cause.getMessage(), cause);
        myRaisedValue = raisedValue;
    }

    @Override
    void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        FusionIo.write(eval, out, myRaisedValue);
        super.displayMessage(eval, out);
    }

    @Override
    public final Object getRaisedValue()
    {
        return myRaisedValue;
    }
}
