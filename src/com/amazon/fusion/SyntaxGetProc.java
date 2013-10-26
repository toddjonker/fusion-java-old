// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionIo.safeWrite;
import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import static com.amazon.fusion.FusionVoid.voidValue;

final class SyntaxGetProc
    extends Procedure
{
    SyntaxGetProc()
    {
        //    "                                                                               |
        super("Traverses down through syntax objects.\n" +
              "CONTAINER must be syntax container (struct, list, or sexp).\n" +
              "Each PART must be a string, symbol, or int, to denote either a struct's\n" +
              "field-name or a sequence's index.",
              "container", "part", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        SyntaxContainer stx = checkSyntaxContainerArg(0, args);
        SyntaxValue value = stx;

        final int lastArg = args.length - 1;
        for (int i = 1; i <= lastArg; i++)
        {
            switch (stx.getType())
            {
                case LIST:
                case SEXP:
                {
                    int index = checkIntArg(i, args);
                    SyntaxSequence s = (SyntaxSequence) stx;
                    if (s.size() <= index)
                    {
                        return voidValue(eval);
                    }
                    value = s.get(eval, index);
                    break;
                }
                case STRUCT:
                {
                    String field = checkTextArg(i, args);
                    SyntaxStruct s = (SyntaxStruct) stx;
                    value = s.get(eval, field);
                    break;
                }
                default:
                {
                    throw new IllegalStateException();
                }
            }

            if (value == null) return voidValue(eval);

            if (i < lastArg)
            {
                try
                {
                    stx = (SyntaxContainer) value;
                }
                catch (ClassCastException cce)
                {
                    StringBuilder out = new StringBuilder();
                    out.append("expected syntax container before traversing ");
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
