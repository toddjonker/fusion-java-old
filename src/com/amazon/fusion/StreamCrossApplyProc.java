package com.amazon.fusion;





/**
 * Applies a predicate to a stream and generates a flat-structured stream
 */
final class StreamCrossApplyProc
    extends Procedure
{
    StreamCrossApplyProc()
    {
        super("Applies a predicate to a stream and" +
              " generates a flat-structured stream");
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
        throws FusionException
    {

        checkArityExact(2, args);

        Stream source = checkStreamArg(0,args);
        Procedure proc = checkProcArg(1,args);
        return new CrossApplyStream(eval,proc,source);
    }
}
