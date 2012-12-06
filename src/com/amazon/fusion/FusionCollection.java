// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;


final class FusionCollection
{
    private FusionCollection() {}


    //========================================================================
    // Predicates


    static boolean isCollection(Evaluator eval, Object v)
    {
        return (v instanceof BaseCollection);
    }


    //========================================================================
    // Accessors


    /**
     * @return not null.
     */
    static String[] unsafeCollectionAnnotationStrings(Evaluator eval,
                                                      Object vector)
    {
        return ((BaseCollection) vector).myAnnotations;
    }


    static int unsafeCollectionSize(Evaluator eval, Object collection)
        throws FusionException
    {
        return ((BaseCollection) collection).size();
    }


    //========================================================================


    abstract static class BaseCollection
        extends FusionValue
        implements Writeable
    {
        /** Not null */
        final String[] myAnnotations;

        BaseCollection()
        {
            myAnnotations = EMPTY_STRING_ARRAY;
        }

        BaseCollection(String[] annotations)
        {
            assert annotations != null;
            myAnnotations = annotations;
        }

        abstract int size()
            throws FusionException;
    }
}
