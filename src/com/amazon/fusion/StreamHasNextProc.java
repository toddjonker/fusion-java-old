package com.amazon.fusion;



/**
 * Checks whether there are FusionValues left to fetch in the stream
 */
final class StreamHasNextProc
    extends Procedure
{
    StreamHasNextProc()
    {
        super("Returns whether there are FusionValues left to fetch in the stream");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        Stream source = checkStreamArg(0, args);

        boolean hasNext = source.hasNext();
        return eval.newBool(hasNext);
    }
}
