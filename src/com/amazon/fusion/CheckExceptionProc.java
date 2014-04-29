// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.makeString;
import static com.amazon.fusion.FusionString.stringToJavaString;
import static com.amazon.fusion.FusionVoid.voidValue;


final class CheckExceptionProc
    extends Procedure2
{
    CheckExceptionProc()
    {
        //    "                                                                               |
        super("NOT FOR APPLICATION USE.",
              "tag", "thunk");
    }


    Class<? extends Exception> classFor(String tag)
    {
        switch (tag)
        {
            case "any":      return FusionException.class;
            case "argument": return ArgumentException.class;
            case "arity":    return ArityFailure.class;
            case "check":    return CheckException.class;
            case "contract": return ContractException.class;
            case "result":   return ResultFailure.class;
            case "syntax":   return SyntaxException.class;
            default:         return null;
        }
    }


    String descriptionFor(Exception e)
    {
        if (e instanceof FusionException)
        {
            if (e instanceof ContractException)
            {
                if (e instanceof ArgumentException) return "argument exception";
                if (e instanceof ArityFailure)      return "arity exception";
                if (e instanceof ResultFailure)     return "result exception";

                return "contract exception";
            }

            if (e instanceof CheckException)  return "check exception";
            if (e instanceof SyntaxException) return "syntax exception";

            return "other Fusion exception";
        }

        return e.getClass().getName();
    }


    @Override
    Object doApply(Evaluator eval, Object tag, Object thunk)
        throws FusionException
    {
        try
        {
            Procedure proc = (Procedure) thunk;
            eval.callNonTail(proc);
        }
        catch (Exception e)
        {
            Class<? extends Exception> expected =
                classFor(stringToJavaString(eval, tag));

            if (expected.isInstance(e))
            {
                // Successful check
                return voidValue(eval);
            }

            return makeString(eval, descriptionFor(e));
        }

        return makeString(eval, "");
    }
}
