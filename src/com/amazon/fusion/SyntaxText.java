// Copyright (c) 2012-2013 Amazon.com, Inc. All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionText.BaseText;

abstract class SyntaxText
    extends SimpleSyntaxValue
{
    /**
     * @param datum must not be null.
     */
    SyntaxText(SourceLocation loc, BaseText datum)
    {
        super(loc, datum);
    }


    final String stringValue()
    {
        return ((BaseText) myDatum).stringValue();
    }
}
