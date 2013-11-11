// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class ValuesProc
    extends Procedure
{
    ValuesProc()
    {
        //    "                                                                               |
        super("Returns all the given `value`s; that is, returns zero, one, or multiple values.\n" +
              "Usually used in conjuction with `let_values` to bind the results to names.",
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
