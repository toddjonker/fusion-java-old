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
            IonValue iv = value.getDom();
            if (iv instanceof IonSequence)
            {
                return new DomStream((IonSequence) iv);
            }
        }

        throw new ContractFailure("value is not streamable: " + value.write());
     }


    static boolean allHaveNext(Stream... streams)
    {
        for (Stream s : streams)
        {
            if (! s.hasNext()) return false;
        }
        return true;
    }
}
