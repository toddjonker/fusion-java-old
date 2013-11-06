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
                                                      Object collection)
    {
        return ((BaseCollection) collection).myAnnotations;
    }


    static int unsafeCollectionSize(Evaluator eval, Object collection)
        throws FusionException
    {
        return ((BaseCollection) collection).size();
    }


    //========================================================================
    // Modifiers


    /**
     * @param collection must be a collection.
     * @param annotations must no elements that are null or empty.
     */
    static Object unsafeCollectionAnnotate(Evaluator eval,
                                           Object collection,
                                           String[] annotations)
        throws FusionException
    {
        return ((BaseCollection) collection).annotate(eval, annotations);
    }


    //========================================================================


    abstract static class BaseCollection
        extends FusionValue
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

        abstract Object annotate(Evaluator eval, String[] annotations)
            throws FusionException;
    }
}
