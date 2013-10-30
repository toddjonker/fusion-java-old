// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonText;

/**
 *
 */
final class FusionText
{
    private FusionText() {}


    /**
     * @return null if given {@code null.string} or {@code null.symbol}.
     */
    static String unsafeTextToString(Evaluator eval, Object stringOrSymbol)
        throws FusionException
    {
        return ((IonText) stringOrSymbol).stringValue();
    }
}
