// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionSequence.isSequence;
import static com.amazon.fusion.FusionSequence.unsafeSequenceDot;
import static com.amazon.fusion.FusionStruct.isStruct;
import static com.amazon.fusion.FusionStruct.unsafeStructDot;
import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import static com.amazon.fusion.FusionVoid.isVoid;
import static com.amazon.fusion.FusionVoid.voidValue;
import static com.amazon.fusion.FusionWrite.safeWrite;

final class DotProc
    extends Procedure
{
    DotProc()
    {
        //    "                                                                               |
        super("Traverses down through a data structure.\n" +
              "The `collection` must be a struct, list, or sexp.\n" +
              "Each PART must be a string, symbol, or int, to denote either a struct's\n" +
              "field-name or a sequence's index. If any part doesn't have a matching value in\n" +
              "its container (the list index is out of bounds, or the field doesn't exist),\n" +
              "then the result is void and any further parts are not traversed.",
              "collection", "part", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);

        Object c = args[0];

        boolean cIsSequence = isSequence(eval, c);
        boolean cIsStruct   = isStruct(eval, c);
        if (! (cIsSequence || cIsStruct))
        {
            throw argFailure("collection", 0, args);
        }

        Object value = c;

        final int lastArg = args.length - 1;
        for (int i = 1; i <= lastArg; i++)
        {
            if (cIsSequence)
            {
                int pos = checkIntArg(i, args);
                value = unsafeSequenceDot(eval, c, pos);
            }
            else
            {
                String field = checkTextArg(i, args);
                value = unsafeStructDot(eval, c, field);
            }

            if (isVoid(eval, value)) return value;

            if (value == null) return voidValue(eval);

            if (i < lastArg)
            {
                cIsSequence = isSequence(eval, value);
                cIsStruct   = isStruct(eval, value);
                if (cIsSequence || cIsStruct)
                {
                    c = value;
                }
                else
                {
                    StringBuilder out = new StringBuilder();
                    out.append("expected collection before traversing ");
                    writeFriendlyIndex(out, i + 1);
                    out.append(" argument, had: ");
                    safeWrite(eval, out, value);
                    out.append("\nArguments were:");
                    for (Object arg : args)
                    {
                        out.append("\n  ");
                        safeWrite(eval, out, arg);
                    }
                    String message = out.toString();
                    throw contractFailure(message);
                }
            }
        }

        return value;
    }

}
