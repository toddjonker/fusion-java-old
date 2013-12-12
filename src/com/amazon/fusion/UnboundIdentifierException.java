// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Indicates a reference to an unbound identifier.
 */
@SuppressWarnings("serial")
public final class UnboundIdentifierException
    extends SyntaxException
{
    private final String myText;


    /**
     * @param identifier must not be null.
     */
    UnboundIdentifierException(SyntaxSymbol identifier)
    {
        super(null, "unbound identifier", identifier);
        myText = identifier.stringValue();
    }


    /**
     * Gets the text of the unbound identifier.
     */
    public String getIdentifierString()
    {
        return myText;
    }
}
