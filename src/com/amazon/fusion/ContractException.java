// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Indicates inappropriate run-time use of a procedure or syntactic form;
 * that is, a failure to satisfy a feature's contract.
 */
@SuppressWarnings("serial")
public class ContractException
    extends FusionException
{
    /**
     * @param message
     */
    ContractException(String message)
    {
        super(message);
    }
}
