// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionList.checkNullableListArg;
import static com.amazon.fusion.FusionList.unsafeListAppendM;


final class AppendMProc
    extends Procedure
{
    AppendMProc()
    {
        //    "                                                                               |
        super("Concatenates the `list`s, mutating the first argument when possible.  If the\n" +
              "first argument cannot be stretched, a fresh list is made, similar to the first.\n" +
              "Any argument that is `null.list` is treated as if it's `[]`.",
              "list", DOTDOTDOTPLUS);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        int arity = args.length;

        Object first = checkNullableListArg(eval, this, 0, args);

        if (arity == 1) return first;

        Object[] listArgs = new Object[arity - 1];

        for (int i = 1; i < arity; i++)
        {
            listArgs[i - 1] = checkNullableListArg(eval, this, i, args);
        }

        return unsafeListAppendM(eval, first, listArgs);
    }
}
