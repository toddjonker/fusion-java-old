// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class ValuesProc
    extends Procedure
{
    ValuesProc()
    {
        //    "                                                                               |
        super("Produces [multiple results](fusion/procedure.html#results), returning the zero\n" +
              "or more `value`s. Usually used in conjuction with `let_values` to bind the\n" +
              "results to names.",
              "value", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args) throws FusionException
    {
        int arity = args.length;

        switch (arity)
        {
            case 0:
            {
                return FusionUtils.EMPTY_OBJECT_ARRAY;
            }
            case 1:
            {
                return args[0];
            }
            default:
            {
                return args;
            }
        }
    }
}
