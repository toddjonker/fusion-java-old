// Copyright (c) 2017 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * This class is an internal procedure used by the with_handlers form.
 * It evaluates a thunk, catches FusionExceptions returned by the evaluation,
 *   and applies the exception value to the cond expression passed in.
 *
 * It is NOT an implementation of Racket's call-with-exception-handler which
 *   evaluates a thunk with a given exception handler.
 */
final class CallWithHandlerProc
    extends Procedure2
{
    @Override
    Object doApply(Evaluator eval, Object thunk, Object handler)
        throws FusionException
    {
        try
        {
            Procedure thunker = (Procedure) thunk;
            return eval.callNonTail(thunker);
        }
        catch (FusionException e)
        {
            Procedure h = (Procedure) handler;

            Object raised = e.getRaisedValue();
            return eval.bounceTailCall(h, raised);
        }
    }
}
