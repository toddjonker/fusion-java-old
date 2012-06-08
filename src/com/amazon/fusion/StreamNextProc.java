package com.amazon.fusion;


/**
 * Returns the next fusion value in the stream
 */
final class StreamNextProc
    extends Procedure
{
    StreamNextProc()
    {
        super("Returns the next fusion value in the stream");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        Stream source = checkStreamArg(0,args);
        if (source.hasNext())
        {
            return source.next();
        }
        throw contractFailure("No new items to fetch from stream");
    }
}
