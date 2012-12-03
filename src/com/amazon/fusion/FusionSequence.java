// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.FusionCollection.BaseCollection;


final class FusionSequence
{
    private FusionSequence() {}


    //========================================================================
    // Predicates


    static boolean isSequence(Evaluator eval, Object v)
    {
        return (v instanceof BaseSequence);
    }


    //========================================================================
    // Accessors

    /**
     * @param sequence must be a sequence.
     * @return void if the position is out of bounds.
     */
    static Object unsafeSequenceDot(Evaluator eval, Object sequence, int pos)
        throws FusionException
    {
        return ((BaseSequence) sequence).dot(eval, pos);
    }


    //========================================================================


    abstract static class BaseSequence
        extends BaseCollection
    {
        BaseSequence() {}

        BaseSequence(String[] annotations)
        {
            super(annotations);
        }

        /** Returns void if the position is out of bounds. */
        abstract Object dot(Evaluator eval, int pos)
            throws FusionException;

        /** Throws if the position is out of bounds. */
        abstract Object unsafeRef(Evaluator eval, int pos)
            throws FusionException;
    }
}
