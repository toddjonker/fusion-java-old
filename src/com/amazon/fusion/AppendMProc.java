// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.checkNullableListArg;
import static com.amazon.fusion.FusionList.unsafeListAppendM;


final class AppendMProc
    extends Procedure
{
    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        int arity = args.length;

        Object first = checkNullableListArg(eval, this, 0, args);

        Object[] listArgs = new Object[arity - 1];

        for (int i = 1; i < arity; i++)
        {
            listArgs[i - 1] = checkNullableListArg(eval, this, i, args);
        }

        return unsafeListAppendM(eval, first, listArgs);
    }
}
