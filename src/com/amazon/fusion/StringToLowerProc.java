// Copyright (c) 2012 Amazon.com, Inc. All rights reserved

package com.amazon.fusion;

/*
 * Converts all the characters in a string to lower-case letters
 */
final class StringToLowerProc
    extends Procedure
{
    StringToLowerProc()
    {
        super("Converts all the characters in a STRING to lower-case letters",
              "string");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        String input = checkTextArg(0, args);

        if (input == null)
        {
            throw contractFailure("Input is null; transformation cannot be performed");
        }

        return eval.newString(input.toLowerCase());
    }
}
