// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.isStruct;
import static com.amazon.fusion.FusionStruct.unsafeStructRemoveKey;
import com.amazon.ion.IonStruct;

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

        Object struct = args[0];

        boolean isStruct = isStruct(eval, struct);
        if (! isStruct)
        {
            checkStructArg(0, args);
        }

        if (args.length == 1) return struct;


        String[] keys = new String[args.length - 1];
        for (int i = 1; i < args.length; i++)
        {
            keys[i-1] = checkTextArg(i, args);
        }

        if (isStruct)
        {
            return unsafeStructRemoveKey(eval, struct, keys);
        }
        else
        {
            IonStruct result = ((IonStruct) struct).cloneAndRemove(keys);
            return eval.inject(result);
        }
    }
}
