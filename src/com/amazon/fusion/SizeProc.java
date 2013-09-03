// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionCollection.unsafeCollectionSize;

final class SizeProc
    extends Procedure1
{
    SizeProc()
    {
        //    "                                                                               |
        super("Returns the number of elements in the `collection`.\n" +
              "The size of `null.list` (_etc._) is zero.  If `collection` is an improper sexp,\n" +
              "an exception is thrown.",
              "collection");
    }

    @Override
    Object doApply(Evaluator eval, Object arg)
        throws FusionException
    {
        checkCollectionArg(eval, 0, arg);
        int size = unsafeCollectionSize(eval, arg);
        return eval.newInt(size);
    }
}
