// Copyright (c) 2012 Amazon.com, Inc. All rights reserved.

package com.amazon.fusion;

abstract class SyntaxText
    extends SyntaxValue
{
    protected final String myText;

    /**
     * @param anns the new instance assumes ownership of the array and
     * it must not be modified later. Must not be null.
     * @param text may be null.
     */
    SyntaxText(String text, String[] anns, SourceLocation loc)
    {
        super(anns, loc);
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
