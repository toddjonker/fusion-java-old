// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;
import java.util.Arrays;
import java.util.Iterator;

/**
 * Utility methods for working with {@link FusionIterator}s.
 */
final class Iterators
{
    private Iterators() { }


    static Object iterate(Object[] array)
    {
        // TODO optimize this
        return new IteratorAdaptor(Arrays.asList(array).iterator());
    }


    /**
     * Builds a Fusion iterator over the contents of an Ion sequence.
     */
    static Object iterateIonSequence(IonSequence seq)
    {
        return new IonIteratorAdaptor(seq.iterator());
    }

    // TODO iterateIonReader


    //========================================================================


    private static class AbstractIterator
        extends FusionIterator
    {
        AbstractIterator()
        {
            super(null, null);
        }

        @Override
        Object doHasNextTail(Evaluator eval)
            throws FusionException
        {
            return eval.newBool(hasNext(eval));
        }

        @Override
        Object doNextTail(Evaluator eval)
            throws FusionException
        {
            return next(eval);
        }
    }


    private static final class IteratorAdaptor
        extends AbstractIterator
    {
        private final Iterator<?> myIterator;

        IteratorAdaptor(Iterator<?> iter)
        {
            myIterator = iter;
        }

        @Override
        public boolean hasNext(Evaluator eval)
        {
            return myIterator.hasNext();
        }

        @Override
        public Object next(Evaluator eval)
        {
            return eval.inject(myIterator.next());
        }
    }


    /** Custom class avoid some dynamic dispatch. */
    private static final class IonIteratorAdaptor
        extends AbstractIterator
    {
        private final Iterator<IonValue> myIonIterator;

        IonIteratorAdaptor(Iterator<IonValue> iter)
        {
            myIonIterator = iter;
        }

        @Override
        public boolean hasNext(Evaluator eval)
        {
            return myIonIterator.hasNext();
        }

        @Override
        public Object next(Evaluator eval)
        {
            return new DomValue(myIonIterator.next());
        }
    }
}
