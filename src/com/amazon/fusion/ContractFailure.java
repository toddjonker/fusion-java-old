// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Indicates inappropriate run-time use of a procedure or syntactic form.
 */
@SuppressWarnings("serial")
public class ContractFailure
    extends FusionException
{
    /**
     * @param message
     */
    ContractFailure(String message)
    {
        super(message);
    }
}
