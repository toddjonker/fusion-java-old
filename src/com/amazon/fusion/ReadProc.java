// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVoid.voidValue;
import com.amazon.ion.IonValue;
import java.util.Iterator;


final class ReadProc
    extends Procedure
{
    private Iterator<IonValue> myInputValues;

    ReadProc()
    {
        //    "                                                                               |
        super("Reads an Ion value from the standard input stream.  Returns void when there's\n" +
              "no more data.");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        if (myInputValues == null)
        {
            myInputValues = eval.getSystem().iterate(System.in);
        }

        Object result;
        if (myInputValues.hasNext())
        {
            IonValue v = myInputValues.next();
            result = eval.inject(v);
        }
        else
        {
            result = voidValue(eval);
        }

        return result;
    }
}
