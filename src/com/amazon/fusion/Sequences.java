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
    private static final class DomStream extends Stream
    {
        private final Iterator<IonValue> myIonIterator;

        DomStream(IonSequence seq)
        {
            myIonIterator = seq.iterator();
        }

        DomStream(Iterator<IonValue> iter)
        {
            myIonIterator = iter;
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

    }


    static Stream streamFor(FusionValue value)
        throws ContractFailure
    {
        if (value instanceof DomValue)
        {
            IonValue iv = FusionValue.toIonValue(value);
            if (iv instanceof IonSequence)
            {
                return new DomStream((IonSequence) iv);
            } else if (value instanceof Stream)
            {
                return (Stream)value;
            }
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
