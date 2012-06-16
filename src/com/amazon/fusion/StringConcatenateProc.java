// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Concatenates 0 or more strings and returns the result
 */
class StringConcatenateProc
    extends Procedure
{
    StringConcatenateProc()
    {
        //    "                                                                               |
        super("Concatenates 0 or more strings and returns the result");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        String result = "";

        for (int i = 0; i < args.length; i++)
        {
            String v;
            try
            {
                v = checkStringArg(i, args);
            } catch (ArgTypeFailure af)
            {
                v = checkTextArg(i, args);
            }
            result += v;
        }

        return eval.newString(result);
    }
}
