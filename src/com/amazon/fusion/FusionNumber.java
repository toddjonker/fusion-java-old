// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonInt;

/**
 *
 */
final class FusionNumber
{
    private FusionNumber() {}


    /**
     * @param fusionInt must be a non-null int.
     */
    static int unsafeTruncateToInt(Evaluator eval, Object fusionInt)
        throws FusionException
    {
        return ((IonInt) fusionInt).intValue();
    }
}
