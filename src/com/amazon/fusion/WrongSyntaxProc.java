// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonValue;

/**
 *
 */
public class WrongSyntaxProc
    extends Procedure
{
    WrongSyntaxProc()
    {
        //    "                                                                               |
        super("Raises a syntax error located at the given STX syntax. The MESSAGEs are\n" +
              "displayed as part of the error.",
              "stx", "message", DOTDOTDOT);
    }

    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        IonValue stx = checkIonArg(0, args);

        String name = null; // TODO infer name
        String message = FusionValue.displayManyToString(args, 1);

        throw new SyntaxFailure(name, message, stx);
    }
}
