package com.amazon.fusion;





/**
 * Generates the stream to represent the cross product of 2 streams
 */
final class StreamCrossProductProc
    extends Procedure
{
    StreamCrossProductProc()
    {
        super("Generates the stream to represent the " +
              "cross product of 2 streams");
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
        throws FusionException
    {

        checkArityExact(2, args);

        Stream source1 = checkStreamArg(0,args);
        Stream source2 = checkStreamArg(1,args);
        return new CrossProductStream(eval,source1,source2);
    }
}
