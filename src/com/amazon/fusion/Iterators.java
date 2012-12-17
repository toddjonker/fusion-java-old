// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;
import java.util.Iterator;

/**
 * Utility methods for working with {@link FusionIterator}s.
 */
final class Iterators
{
    private Iterators() { }


    /** Iterated values must not need injecting. */
    static FusionIterator iterate(Iterator<?> iterator)
    {
        return new IteratorAdaptor(iterator);
    }


    /**
     * Builds a Fusion iterator from an IonValue iterator.
     */
    static Object iterateIon(Iterator<IonValue> iterator)
    {
        return new IonIteratorAdaptor(iterator);
    }


    //========================================================================


    /**
     * Base class for Fusion iterators implemented in Java.
     * Subclasses just need to implement hasNext and next, and next must
     * return Fusion values.
     */
    abstract static class AbstractIterator
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

        /** Iterated values must not need injecting. */
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
            return myIterator.next();
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
            // Don't assume that IonValue is a Fusion value.
            // It may need converting to another form.
            return eval.inject(myIonIterator.next());
        }
    }
}
