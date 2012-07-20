// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * Class to return the projection of a stream based on some procedure.
 */
class ProjectStream
    extends Stream
{

    private final Stream mySource;
    private final Procedure proc;
    // TODO FUSION-25 stashing of evaluators is incorrect
    private final Evaluator eval;

    public ProjectStream(Evaluator eval, Procedure proc, Stream source)
    {
        this.eval = eval;
        this.proc = proc;
        this.mySource = source;
    }

    @Override
    boolean hasNext()
        throws FusionException
    {
        return mySource.hasNext();
    }

    @Override
    FusionValue next()
        throws FusionException
    {
        FusionValue fv = mySource.next();
        return eval.applyNonTail(proc, fv);
    }
}
