package com.amazon.fusion;





/**
 * Projects a stream onto another stream w/ use of transformation
 */
final class StreamProjectProc
    extends Procedure
{
    StreamProjectProc()
    {
        super("Projects a stream onto another stream w/ use of transformation");
    }

    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        Stream source = checkStreamArg(0,args);
        Procedure proc = checkProcArg(1,args);
        return new ProjectStream(eval,proc,source);
    }
}
