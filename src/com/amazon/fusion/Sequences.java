// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;
import java.util.Iterator;

/**
 *
 */
class Sequences
{
    private static final class DomIterator implements Iterator<FusionValue>
    {
        private final Iterator<IonValue> myIonIterator;

        DomIterator(IonSequence seq)
        {
            myIonIterator = seq.iterator();
        }

        @Override
        public boolean hasNext()
        {
            return myIonIterator.hasNext();
        }

        @Override
        public FusionValue next()
        {
            return new DomValue(myIonIterator.next());
        }

        @Override
        public void remove()
        {
            throw new UnsupportedOperationException();
        }
    }


    static Iterator<FusionValue> iteratorFor(FusionValue value)
        throws ContractFailure
    {
        if (value instanceof DomValue)
        {
            IonValue iv = value.getDom();
            if (iv instanceof IonSequence)
            {
                return new DomIterator((IonSequence) iv);
            }
        }

        throw new ContractFailure("value is not iterable: " + value.write());
    }
}
