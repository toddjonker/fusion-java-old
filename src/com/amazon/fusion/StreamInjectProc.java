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
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        // TODO FUSION-41 support non-FV
        InjectStream injectStream = new InjectStream( (FusionValue) args[0]);

        return injectStream;
    }
}
