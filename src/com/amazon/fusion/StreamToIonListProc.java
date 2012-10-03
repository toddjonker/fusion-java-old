// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.util.ArrayList;

/**
 * Converts a stream to an ion list
 */
final class StreamToIonListProc
    extends Procedure
{
    StreamToIonListProc()
    {
        super("Converts a STREAM to an ion list.",
              "stream");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        // check if first arg is of stream class
        Stream source = checkStreamArg(0,args);

        ArrayList<Object> children = new ArrayList<Object>();

        while (source.hasNext())
        {
            Object fv = source.next();
            children.add(fv);
        }

        return FusionVector.makeVectorFrom(eval, children);
    }
}
