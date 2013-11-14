// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSymbol;


final class FusionSymbol
{
    private FusionSymbol() {}


    static String checkNullableArg(Procedure who, int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonSymbol iv = who.checkDomArg(IonSymbol.class, "symbol",
                                       true /* nullable */, argNum, args);
        return iv.stringValue();
    }

    static String checkRequiredArg(Procedure who, int argNum, Object... args)
        throws ArgTypeFailure
    {
        IonSymbol iv = who.checkDomArg(IonSymbol.class, "non-null symbol",
                                       false /* nullable */, argNum, args);
        return iv.stringValue();
    }


    static final class ToStringProc
        extends Procedure
    {
        ToStringProc()
        {
            //    "                                                                               |
            super("Converts a `symbol` to a string with the same text. Returns `null.string` when\n"
                + "given `null.symbol`.",
                  "symbol");
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            String input = checkNullableArg(this, 0, args);
            return eval.newString(input);
        }
    }
}
