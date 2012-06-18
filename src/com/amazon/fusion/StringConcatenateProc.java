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
        StringBuilder resultBuilder = new StringBuilder();

        for (int i = 0; i < args.length; i++)
        {
            String v = checkTextArg(i, args);
            resultBuilder.append(v);
        }

        return eval.newString(resultBuilder.toString());
    }
}
