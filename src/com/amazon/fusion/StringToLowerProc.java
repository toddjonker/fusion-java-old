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
        super("Converts all the characters in a string to lower-case letters");
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        String input = checkTextArg(0, args);

        if (input == null)
        {
            throw contractFailure("Input is null; transformation cannot be performed");
        }

        return eval.newString(input.toLowerCase());
    }
}
