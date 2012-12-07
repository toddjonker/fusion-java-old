// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.unsafeStructRemoveKey;

final class RemoveKeysProc
    extends Procedure
{
    RemoveKeysProc()
    {
        //    "                                                                               |
        super("Returns a struct derived from `struct` without fields with the given `name`s.",
              "struct", "name", DOTDOTDOT);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);

        Object struct = checkStructArg(eval, 0, args);

        if (args.length == 1) return struct;

        String[] keys = new String[args.length - 1];
        for (int i = 1; i < args.length; i++)
        {
            keys[i-1] = checkTextArg(i, args);
        }

        return unsafeStructRemoveKey(eval, struct, keys);
    }
}
