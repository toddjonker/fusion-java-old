// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Converts an ion list to a stream
 * Class serves as a wrapper for {@link Sequences}.streamFor()
 */
final class IonListToStreamProc
    extends Procedure1
{
    IonListToStreamProc()
    {
        super("Converts a LIST to a stream",
              "list");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        Object list = checkListArg(0, arg);

        return Sequences.streamFor(list);
    }
}
