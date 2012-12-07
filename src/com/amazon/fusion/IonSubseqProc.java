// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionVector.unsafeVectorSize;
import static com.amazon.fusion.FusionVector.unsafeVectorSubseq;

final class IonSubseqProc
    extends Procedure
{
    IonSubseqProc()
    {
        //    "                                                                               |
        super("Returns a list holding the elements from `list` between positions\n" +
              "`from` and `to`.  The following precondition applies:\n" +
              "\n" +
              "    0 <= from <= to <= (size list)\n" +
              "\n" +
              "The result may share structure with `list`.",
              "list", "from", "to");
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        Object vector = checkListArg(eval, 0, args);

        int size = unsafeVectorSize(eval, vector);

        int from = checkIntArg(1, args);
        int to   = checkIntArg(2, args);

        if (to < from)
        {
            throw new ArgTypeFailure(this, "'from' less than 'to'",
                                     1, args);
        }
        if (size < to)
        {
            throw new ArgTypeFailure(this, "'to' less than (size sequence)",
                                     2, args);
        }

        assert from <= to && to <= size;

        if (from == 0 && to == size) return vector;

        int len = to - from;
        return unsafeVectorSubseq(eval, vector, from, len);
    }
}
