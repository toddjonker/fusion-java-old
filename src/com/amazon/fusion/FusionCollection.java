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
     * @param annotations must not be null and must not contain elements
     * that are null or empty. This method assumes ownership of the array
     * and it must not be modified later.
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
        implements Annotated
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

        @Override
        public String[] annotationsAsJavaStrings()
        {
            return myAnnotations;
        }

        abstract int size()
            throws FusionException;

        /**
         * @param annotations must not be null and must not contain elements
         * that are null or empty. This method assumes ownership of the array
         * and it must not be modified later.
         */
        abstract Object annotate(Evaluator eval, String[] annotations)
            throws FusionException;
    }
}
