// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Class to express streams for the select operator
 */
final class SelectStream
    extends Stream
{
    private final Stream mySource;
    private final Procedure proc;
    // TODO FUSION-25 stashing of evaluators is incorrect
    private final Evaluator eval;
    private FusionValue nextResult;

    public SelectStream(Evaluator eval, Procedure proc, Stream source)
    {
        this.eval = eval;
        this.proc = proc;
        this.mySource = source;
    }

    @Override
    boolean hasNext()
        throws FusionException
    {
        return search();
    }

    boolean search()
        throws FusionException
    {
        if (mySource.hasNext())
        {
            FusionValue fv = this.mySource.next();
            Object boolResult = eval.callNonTail(proc, fv);
            Boolean bv = FusionValue.asBoolean(boolResult);
            if (bv != null)
            {
                boolean boolVal = bv.booleanValue();
                nextResult = boolVal ? fv : null;
                return (nextResult != null) ? true : this.search();
            }
            else
            {
                throw new ContractFailure("Proc does not return true or false");
            }
        }
        return false;
    }

    @Override
    FusionValue next()
        throws FusionException
    {
        if (nextResult == null)
        {
            if (!search())
            {
                throw new FusionException("No new elements to fetch from stream");
            }
        }
        FusionValue rv = nextResult;
        nextResult = null;
        return rv;
    }

}
