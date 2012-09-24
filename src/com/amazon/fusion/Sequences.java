// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSequence;
import com.amazon.ion.IonValue;
import java.util.Iterator;

/**
 * Implements StreamFor and DomStream
 */
class Sequences
{
    private static final class DomStream extends Stream
    {
        private final Iterator<?> myIonIterator;

        DomStream(IonSequence seq)
        {
            myIonIterator = seq.iterator();
        }

        DomStream(Iterator<?> iter)
        {
            myIonIterator = iter;
        }

        @Override
        public boolean hasNext()
        {
            return myIonIterator.hasNext();
        }

        @Override
        public Object next()
        {
            Object next = myIonIterator.next();
            return new DomValue((IonValue) next);
        }

    }


    static Stream streamFor(Object value)
        throws ContractFailure
    {
        IonValue iv = FusionValue.toIonValue(value);
        if (iv instanceof IonSequence)
        {
            return new DomStream((IonSequence) iv);
        }

        if (value instanceof Stream)
        {
            return (Stream)value;
        }

        throw new ContractFailure("value is not streamable: "
                                  + FusionValue.writeToString(value));
    }

    static Stream streamFor(IonSequence ionSeq)
        throws ContractFailure
    {
        return Sequences.streamFor(new DomValue(ionSeq));
    }

    static Stream streamFor(Iterator<IonValue> iter)
    {
        return new DomStream(iter);
    }

    static boolean allHaveNext(Stream... streams)
        throws FusionException
    {
        for (Stream s : streams)
        {
            if (! s.hasNext()) return false;
        }
        return true;
    }

}
