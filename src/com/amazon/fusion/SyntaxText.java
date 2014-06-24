// Copyright (c) 2012-2013 Amazon.com, Inc. All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionText.BaseText;

abstract class SyntaxText
    extends SimpleSyntaxValue
{
    /**
     * @param datum must not be null.
     */
    SyntaxText(SourceLocation loc, Object[] properties, BaseText datum)
    {
        super(loc, properties, datum);
    }


    final String stringValue()
    {
        return ((BaseText) myDatum).stringValue();
    }
}
