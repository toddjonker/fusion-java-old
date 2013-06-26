// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;
import com.amazon.ion.IonReader;
import com.amazon.ion.IonValue;


final class ReadProc
    extends Procedure
{
    private final DynamicParameter myCurrentIonReaderParam;

    public ReadProc(Object currentIonReaderParam)
    {
        //    "                                                                               |
        super("Reads an Ion value from the current Ion input stream.  Returns void when\n" +
              "there's no more data.");

        myCurrentIonReaderParam = (DynamicParameter) currentIonReaderParam;
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonReader r = myCurrentIonReaderParam.currentValue(eval);

        Object result;
        if (r.next() != null)
        {
            IonValue v = eval.getSystem().newValue(r);
            result = eval.inject(v);
        }
        else
        {
            result = voidValue(eval);
        }

        return result;
    }
}
