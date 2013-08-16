// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 * Represents conditions raised within Fusion code, as opposed to failures
 * within the interpreter implementation.
 */
@SuppressWarnings("serial")
public class FusionException
    extends Exception
{
    // Constructors aren't public because I don't want applications to create
    // exceptions directly or subclass them.

    FusionException(String message)
    {
        super(message);
    }

    FusionException(String message, Throwable cause)
    {
        super(message, cause);
    }

    FusionException(Throwable cause)
    {
        super(cause.getMessage(), cause);
    }


    /**
     * Returns the message string given to the exception constructor.
     * This should be used instead of {@link #getMessage()} since the latter is
     * overridden here to delegate to {@link #displayMessage}.
     */
    final String getBaseMessage()
    {
        return super.getMessage();
    }

    void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
        out.append(super.getMessage());
    }

    /**
     * @return the result of calling {@link #displayMessage}.
     */
    @Override
    public final String getMessage()
    {
        StringBuilder out = new StringBuilder();

        try
        {
            displayMessage(null, out);
        }
        catch (IOException e) {}
        catch (FusionException e) {}

        return out.toString();
    }
}
