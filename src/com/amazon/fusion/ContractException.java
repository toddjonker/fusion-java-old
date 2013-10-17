// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 * Indicates inappropriate run-time use of a procedure or syntactic form;
 * that is, a failure to satisfy a feature's contract.
 */
@SuppressWarnings("serial")
public class ContractException
    extends FusionException
{
    private final SourceLocation myLocation;

    /**
     * @param message
     */
    ContractException(String message)
    {
        super(message);
        myLocation = null;
    }

    /**
     * @param location may be null.
     */
    ContractException(String message, SourceLocation location)
    {
        super(message);
        myLocation = location;
    }

    @Override
    void displayMessage(Evaluator eval, Appendable out)
        throws IOException, FusionException
    {
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
    }
}
