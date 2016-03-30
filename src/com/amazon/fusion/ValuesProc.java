// Copyright (c) 2013-2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


final class ValuesProc
    extends Procedure
{
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
