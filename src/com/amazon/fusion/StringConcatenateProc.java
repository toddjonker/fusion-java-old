// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class StringConcatenateProc
    extends Procedure
{
    StringConcatenateProc()
    {
        //    "                                                                               |
        super("Concatenates the TEXT values (strings or symbols) into a string.  If no \n" +
              "arguments are supplied, the result is `\"\"`.",
              "text", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
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
