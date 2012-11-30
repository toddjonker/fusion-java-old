// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.immutableStruct;
import static com.amazon.fusion.FusionUtils.EMPTY_STRING_ARRAY;


/**
 * Constructs a struct from a sequence of alternating strings and values
 */
final class StructMakeProc
    extends Procedure
{
    StructMakeProc()
    {
        //    "                                                                               |
        super("Constructs a struct from pairs of args, alternating strings and values.");
    }

    void checkArityEven(Object... args)
        throws FusionException
    {
        if ((args.length % 2) == 1)
        {
            String message =
                "Expected even number of args, observed " + args.length;
            throw contractFailure(message);
        }
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityEven(args);

        int fieldCount = (args.length / 2);
        String[] names  = new String[fieldCount];
        Object[] values = new Object[fieldCount];

        int fieldPos = 0;
        for (int i = 0; i < args.length; i++, fieldPos++)
        {
            names [fieldPos] = checkTextArg(i, args);
            values[fieldPos] = args[++i];
        }
        assert fieldPos == fieldCount;

        return immutableStruct(names, values, EMPTY_STRING_ARRAY);
    }
}
