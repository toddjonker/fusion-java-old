// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 *
 */
final class MakeParameterFunction
    extends FunctionValue
{
    MakeParameterFunction()
    {
        //    "                                                                               |
        super("Makes a new dynamic parameter procedure. The initial value of the parameter is\n" +
              "VALUE.",
              "value");
    }


    @Override
    FusionValue invoke(Evaluator eval, FusionValue[] args)
        throws FusionException
    {
        checkArityExact(1, args);
        return new DynamicParameter(args[0]);
    }
}
