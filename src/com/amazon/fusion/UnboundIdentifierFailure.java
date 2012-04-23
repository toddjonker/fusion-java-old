// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;

/**
 * Indicates a reference to an unbound identifier.
 */
@SuppressWarnings("serial")
public class UnboundIdentifierFailure
    extends SyntaxFailure
{
    public UnboundIdentifierFailure(String name, IonValue... exprs)
    {
        super(name, "unbound identifier", exprs);
    }
}
