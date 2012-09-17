// Copyright (c) 2012 Amazon.com, Inc. All rights reserved

package com.amazon.fusion;

/*
 * Converts all the characters in a string to upper-case letters
 */
final class StringToUpperProc
    extends Procedure
{
    StringToUpperProc()
    {
        super("Converts all the characters in a STRING to upper-case letters",
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
            throw contractFailure("Input string is null; transformation cannot be performed");
        }

        return eval.newString(input.toUpperCase());
    }
}
