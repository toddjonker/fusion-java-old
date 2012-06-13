// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonValue;

/**
 * Class to express streams for the select operator
 */
class SelectStream
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
            FusionValue fv;
            fv = this.mySource.next();
            FusionValue [] newArg = { fv };
            FusionValue boolResult = this.eval.applyNonTail(proc,newArg);
            if (boolResult != null)
            {
                IonValue iv = boolResult.ionValue();
                if (iv instanceof IonBool)
                {
                    IonBool ionBool = (IonBool)iv;
                    if (ionBool.isNullValue())
                    {
                        throw new ContractFailure("Only true or false bool vals accepted");
                    }
                    boolean boolVal = ionBool.booleanValue();
                    nextResult = boolVal ? fv : null;
                    return (nextResult != null) ? true : this.search();
                } else
                {
                    throw new ContractFailure("Proc does not return bool values");
                }
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
