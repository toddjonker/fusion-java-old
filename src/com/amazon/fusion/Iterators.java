// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import java.util.Iterator;

/**
 * Utility methods for working with {@link FusionIterator}s.
 *
 * @deprecated Use {@link FusionIterator}.
 */
@Deprecated
final class Iterators  // TODO FUSION-107 remove this class
{
    private Iterators() { }


    /**
     * Builds a Fusion iterator from an IonValue iterator.
     *
     * @deprecated Use {@link FusionIterator#injectIterator(Evaluator,Iterator)}.
     */
    @Deprecated
    static Object iterateIon(Iterator<IonValue> iterator)
    {
        return FusionIterator.injectIterator(null, iterator);
    }
}
