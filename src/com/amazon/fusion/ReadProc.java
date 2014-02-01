// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.eof;
import com.amazon.ion.IonReader;


final class ReadProc
    extends Procedure
{
    private final DynamicParameter myCurrentIonReaderParam;

    public ReadProc(Object currentIonReaderParam)
    {
        //    "                                                                               |
        super("Reads an Ion value from the current Ion input stream.  Returns `eof` when\n" +
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
            result = StandardReader.read(eval, r);
        }
        else
        {
            result = eof(eval);
        }

        return result;
    }
}
