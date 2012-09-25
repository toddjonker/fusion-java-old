// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonSystem;
import com.amazon.ion.IonValue;
import java.util.ArrayList;

/**
 * Class to express the cross product of 2 streams
 */
final class CrossProductStream
    extends Stream
{
    private final Stream mySource1;
    private final Evaluator eval;
    private final ArrayList<Object> cloneSource2;
    private int index;
    private Object first;

    public CrossProductStream(Evaluator eval, Stream source1, Stream source2)
        throws FusionException
    {
        this.mySource1 = source1;
        cloneSource2 = new ArrayList<Object>();
        cloneSourceObjects(source2);

        // TODO FUSION-25 stashing of evaluators is incorrect
        this.eval = eval;
        this.index = 0;
        this.first = null;
    }

    @Override
    boolean hasNext()
        throws FusionException
    {
        return (mySource1.hasNext() || (index != 0));
    }

    // TODO FUSION-26 perform lazy construction of list cache
    // for cross product
    void cloneSourceObjects(Stream originSource)
        throws ContractFailure, FusionException
    {
        while (originSource.hasNext())
        {
            Object fv = originSource.next();
            cloneSource2.add(fv);
        }
    }

    @Override
    Object next()
        throws FusionException
    {
        IonSystem system = eval.getSystem();

        // TODO unionize results
        IonList ionList = system.newEmptyList();
        if (hasNext())
        {
            first = (index > 0) ? first : mySource1.next();
            IonValue ivFirst = FusionValue.copyToIonValue(first, system);
            ionList.add(ivFirst);
            Object second = cloneSource2.get(index);
            IonValue ivSecond = FusionValue.copyToIonValue(second, system);
            ionList.add(ivSecond);
            index = ((index+1) == cloneSource2.size()) ? 0 : index+1;
        }
        else
        {
            throw new ContractFailure("No more args to fetch from stream");
        }
        return new DomValue(ionList);
    }
}
