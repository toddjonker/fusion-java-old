package com.amazon.fusion;



/**
 * Creates a stream for injection
 */
final class StreamInjectProc
    extends Procedure
{
    StreamInjectProc()
    {
        super("Creates a stream for injection");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        InjectStream injectStream = new InjectStream(args[0]);

        return injectStream;

    }
}
