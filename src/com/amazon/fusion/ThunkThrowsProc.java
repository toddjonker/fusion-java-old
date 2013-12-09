// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionString.makeString;


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
        catch (SyntaxException e)
        {
            // Good!  We are expecting this.
            return makeString(eval, "syntax");
        }
        catch (ArgTypeFailure e)
        {
            return makeString(eval, "arg");
        }
        catch (ArityFailure e)
        {
            return makeString(eval, "arity");
        }
        catch (ContractException e)
        {
            return makeString(eval, "contract");
        }
        catch (FusionException e)
        {
            return e;
        }

        return makeBool(eval, false);
    }
}
