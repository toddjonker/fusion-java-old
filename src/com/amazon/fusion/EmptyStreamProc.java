package com.amazon.fusion;



/**
 * Generates an empty stream object
 */
final class EmptyStreamProc
    extends Procedure
{
    private final EmptyStream emptyStream;

    EmptyStreamProc()
    {
        super("Generates an empty stream (equivalent to an empty IonList)");
        this.emptyStream = new EmptyStream();
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(0, args);

        return this.emptyStream;
    }
}
