// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class ThunkThrowsProc
    extends Procedure1
{
    ThunkThrowsProc()
    {
        //    "                                                                               |
        super("XXX",
              "thunk");
    }

    @Override
    Object doApply(Evaluator eval, Object thunk)
        throws FusionException
    {
        // TODO type-check the thunk
        boolean threw;
        try
        {
            Procedure proc = (Procedure) thunk;
            eval.callNonTail(proc);
            threw = false;
        }
        catch (SyntaxFailure e)
        {
            // Good!  We are expecting this.
            threw = true;
        }

        return eval.newBool(threw);
    }
}
