// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.unsafeVectorConcatenateM;


final class ConcatenateMProc
    extends Procedure
{
    ConcatenateMProc()
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

        Object first = coerceListArg(eval, 0, args);

        // TODO FUSION-87 with one arg, conversion above is arguably premature.
        if (arity == 1) return first;

        Object[] vectorArgs = new Object[arity - 1];

        for (int i = 1; i < arity; i++)
        {
            vectorArgs[i - 1] = coerceListArg(eval, i, args);
        }

        return unsafeVectorConcatenateM(eval, first, vectorArgs);
    }
}
