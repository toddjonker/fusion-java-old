// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Indicates a reference to an unbound identifier.
 */
@SuppressWarnings("serial")
public final class UnboundIdentifierFailure
    extends SyntaxFailure
{
    public UnboundIdentifierFailure(String name, SyntaxValue... exprs)
    {
        super(name, "unbound identifier", exprs);
    }
}
