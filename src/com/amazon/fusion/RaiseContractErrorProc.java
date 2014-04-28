// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionString.checkRequiredStringArg;
import static com.amazon.fusion.FusionText.checkRequiredTextArg;


final class RaiseContractErrorProc
    extends Procedure
{
    RaiseContractErrorProc()
    {
        //    "                                                                               |
        super("Raises a general contract failure. " +
              "The `name` text (string or symbol) identifies the procedure.\n" +
              "The `message` string describes the relevant contract failure.",
              "name", "message");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        String name    = checkRequiredTextArg(eval, this, 0, args);
        String message = checkRequiredStringArg(eval, this, 1, args);

        if (name.isEmpty()) name = "unknown procedure";

        throw new ContractException(name + ": " + message);
    }
}
