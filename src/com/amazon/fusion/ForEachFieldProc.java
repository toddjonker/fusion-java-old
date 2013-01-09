// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionStruct.unsafeStructFieldVisit;
import com.amazon.fusion.FusionStruct.StructFieldVisitor;

final class ForEachFieldProc
    extends Procedure
{
    ForEachFieldProc()
    {
        //    "                                                                               |
        super("Applies `proc` to each field within `struct`, ignoring any results.\n" +
              "The `proc` must take two arguments, a field name symbol and a value.\n" +
              "Returns the given `struct`.",
              "proc", "struct");
    }


    @Override
    Object doApply(final Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        final Procedure proc = checkProcArg(0, args);
        Object fs = checkStructArg(eval, 1, args);

        StructFieldVisitor visitor = new StructFieldVisitor()
        {
            @Override
            public Object visit(String name, Object value)
                throws FusionException
            {
                Object nameSym = eval.newSymbol(name);
                eval.callNonTail(proc, nameSym, value);
                return null;
            }
        };

        unsafeStructFieldVisit(eval, fs, visitor);

        return fs;
    }
}
