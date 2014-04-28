// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeInt;
import static com.amazon.fusion.FusionString.checkNullableStringArg;
import java.math.BigInteger;

final class StringToIntProc
    extends Procedure
{
    StringToIntProc()
    {
        //    "                                                                               |
        super("Converts a string to an int.  The string must contain an optional minus sign\n"
            + " (`\"-\"` aka `\"\\x2D\"`) followed by one or more ASCII digits.  Returns\n"
            + "`null.int` when given `null.string`.  An exception is raised if the string is\n"
            + "of invalid format.",
              "int");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        String val = checkNullableStringArg(eval, this, 0, args);

        BigInteger bigInt = (val != null ? parse(val, args) : null);

        return makeInt(eval, bigInt);
    }


    BigInteger parse(String text, Object[] args)
        throws ArgumentException
    {
        try
        {
            // Work-around behavior of BigInteger constructor, which (in
            // defiance of its documentation) accepts leading '+'.
            if (! text.startsWith("+"))
            {
                return new BigInteger(text);
            }
        }
        catch (NumberFormatException e) { }

        throw argFailure("valid int content", 0, args);
    }
}
