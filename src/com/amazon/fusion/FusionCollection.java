// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;


final class FusionCollection
{
    private FusionCollection() {}


    //========================================================================
    // Representation Classes


    abstract static class BaseCollection
        extends BaseValue
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
    }


    //========================================================================
    // Constructors

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
    // Predicates


    static boolean isCollection(Evaluator eval, Object v)
    {
        return (v instanceof BaseCollection);
    }


    //========================================================================
    // Accessors


    static int unsafeCollectionSize(Evaluator eval, Object collection)
        throws FusionException
    {
        return ((BaseCollection) collection).size();
    }


    //========================================================================
    // Procedure Helpers


    /**
     * @param expectation must not be null.
     * @return the Fusion collection, not null.
     */
    static Object checkCollectionArg(Evaluator eval,
                                     Procedure who,
                                     String    expectation,
                                     int       argNum,
                                     Object... args)
        throws FusionException, ArgTypeFailure
    {
        Object arg = args[argNum];
        if (arg instanceof BaseCollection)
        {
            return arg;
        }

        throw who.argFailure(expectation, argNum, args);
    }


    /**
     * @return the Fusion collection, not null.
     */
    static Object checkNullableCollectionArg(Evaluator eval,
                                             Procedure who,
                                             int       argNum,
                                             Object... args)
        throws FusionException, ArgTypeFailure
    {
        String expectation = "nullable collection";
        return checkCollectionArg(eval, who, expectation, argNum, args);
    }


    //========================================================================
    // Procedures


    static final class SizeProc
        extends Procedure1
    {
        SizeProc()
        {
            //    "                                                                               |
            super("Returns the number of elements in the `collection`.\n" +
                  "The size of `null.list` (_etc._) is zero.  If `collection` is an improper sexp,\n" +
                  "an exception is thrown.",
                  "collection");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
            throws FusionException
        {
            checkNullableCollectionArg(eval, this, 0, arg);
            int size = unsafeCollectionSize(eval, arg);
            return makeInt(eval, size);
        }
    }
}
