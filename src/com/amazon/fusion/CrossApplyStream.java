// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;




/**
 * Class to express the cross apply of a stream as a result of the predicate
 * User defined functions can return single elt, list, or stream
 */
final class CrossApplyStream
    extends Stream
{

    private final Stream mySource;
    private Stream myResults;
    private final Procedure proc;
    // TODO FUSION-25 stashing of evaluators is incorrect
    private final Evaluator eval;
    private Object nextResult;

    public CrossApplyStream(Evaluator eval, Procedure proc, Stream source)
    {
        this.eval = eval;
        this.proc = proc;
        this.mySource = source;
        this.myResults = null;
        this.nextResult = null;
    }

    @Override
    boolean hasNext()
        throws FusionException
    {
        if (myResults != null || this.nextResult != null)
        {
            return true;
        }
        return search();
    }

    boolean search()
        throws FusionException
    {
        assert(nextResult == null);
        if (!mySource.hasNext())
        {
            return false;
        }
        Object fv = mySource.next();
        Object rv = eval.callNonTail(proc, fv);
        if (rv instanceof Stream) // defensive checking
        {
             Stream strv = (Stream)rv;
             if (!strv.hasNext())
             {
                 return this.search();
             }
             this.myResults = strv;
             return true;
        }
        else
        {
            myResults = Sequences.streamFor(rv);
            return true;
        }
    }

    @Override
    Object next()
        throws FusionException
    {
        Object rv = null;
        // TODO unionize results / eliminate repeats
        if (nextResult == null && myResults == null)
        {
            if(!search())
            {
                throw new ContractFailure("No new args to fetch from stream");
            }
        }
        if (nextResult != null)
        {
            rv = nextResult;
            nextResult = null;
        }
        else if (myResults != null)
        {
            rv = myResults.next();
            if (!myResults.hasNext())
            {
                myResults = null;
            }
        }
        return rv;
    }
}
