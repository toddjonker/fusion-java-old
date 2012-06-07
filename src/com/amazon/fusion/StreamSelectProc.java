package com.amazon.fusion;



/**
 * Selects elements from a stream if they satisfy some
 * user-defined conditional procedure
 */
final class StreamSelectProc
    extends Procedure
{
    StreamSelectProc()
    {
        super("Selects elements from a stream if they satisfy some " +
              "user-defined conditional procedure.");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(2, args);

        Stream source = checkStreamArg(0,args);
        Procedure proc = checkProcArg(1,args);
        return new SelectStream(eval,proc,source);
    }
}
