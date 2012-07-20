// Copyright (c) 2012 Amazon.com, Inc. All rights reserved.

package com.amazon.fusion;

abstract class SyntaxText
    extends SyntaxValue
{
    protected final String myText;

    /**
     * @param text may be null.
     */
    public SyntaxText(String text)
    {
        myText = text;
    }

    @Override
    protected boolean isNullValue()
    {
        return myText == null;
    }

    protected final String stringValue()
    {
        return myText;
    }
}
