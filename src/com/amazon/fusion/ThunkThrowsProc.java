// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class ThunkThrowsProc
    extends Procedure1
{
    ThunkThrowsProc()
    {
        //    "                                                                               |
        super("NOT FOR APPLICATION USE",
              "thunk");
    }

    @Override
    Object doApply(Evaluator eval, Object thunk)
        throws FusionException
    {
        // TODO type-check the thunk
        try
        {
            Procedure proc = (Procedure) thunk;
            eval.callNonTail(proc);
        }
        catch (SyntaxFailure e)
        {
            // Good!  We are expecting this.
            return eval.newString("syntax");
        }
        catch (ArgTypeFailure e)
        {
            return eval.newString("arg");
        }
        catch (ArityFailure e)
        {
            return eval.newString("arity");
        }
        catch (FusionException e)
        {
            return e;
        }

        return eval.newBool(false);
    }
}
