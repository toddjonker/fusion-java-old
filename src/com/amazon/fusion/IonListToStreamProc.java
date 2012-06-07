package com.amazon.fusion;

import com.amazon.ion.IonList;

/**
 * Converts an ion list to a stream
 * Class serves as a wrapper for {@link Sequences}.streamFor()
 */
final class IonListToStreamProc
    extends Procedure
{
    IonListToStreamProc()
    {
        super("Converts an ion list to a stream");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        // check if first arg is of IonList class
        IonList ionList = checkListArg(0, args);

        return Sequences.streamFor(ionList);
    }
}
