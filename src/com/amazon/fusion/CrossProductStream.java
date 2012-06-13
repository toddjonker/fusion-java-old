// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonList;
import com.amazon.ion.IonValue;
import java.util.ArrayList;

/**
 * Class to express the cross product of 2 streams
 */
class CrossProductStream
    extends Stream
{

    private final Stream mySource1;
    private final Evaluator eval;
    private final ArrayList<FusionValue> cloneSource2;
    private static int index;
    private FusionValue first;

    public CrossProductStream(Evaluator eval, Stream source1, Stream source2)
        throws FusionException
    {
        this.mySource1 = source1;
        cloneSource2 = new ArrayList<FusionValue>();
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
            FusionValue fv = originSource.next();
            cloneSource2.add(fv);
        }
    }

    @Override
    FusionValue next()
        throws FusionException
    {
        FusionValue rv = null;
        // TODO unionize results
        IonList ionList = eval.getSystem().newEmptyList();
        if (hasNext())
        {
            first = (index > 0) ? first : mySource1.next();
            IonValue ivFirst = first.ionValue();
            IonValue cloneFirst = ivFirst.clone();
            ionList.add(cloneFirst);
            FusionValue second = cloneSource2.get(index);
            IonValue ivSecond = second.ionValue();
            IonValue cloneSecond = ivSecond.clone();
            ionList.add(cloneSecond);
            index = ((index+1) == cloneSource2.size()) ? 0 : index+1;
        }
        else
        {
            throw new ContractFailure("No more args to fetch from stream");
        }
        return new DomValue(ionList);
    }
}
