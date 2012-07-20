// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import java.io.IOException;

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
    FusionValue invoke(Evaluator eval, FusionValue[] args)
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
                    long index = checkLongArg(i, args);
                    SyntaxSequence s = (SyntaxSequence) stx;
                    if (s.size() <= index)
                    {
                        return UNDEF;
                    }
                    value = s.get((int) index);
                    break;
                }
                case STRUCT:
                {
                    String field = checkTextArg(i, args);
                    SyntaxStruct s = (SyntaxStruct) stx;
                    value = s.get(field);
                    break;
                }
                default:
                {
                    throw new IllegalStateException();
                }
            }

            if (value == null) return UNDEF;

            if (i < lastArg)
            {
                try
                {
                    stx = (SyntaxContainer) value;
                }
                catch (ClassCastException cce)
                {
                    StringBuilder out = new StringBuilder();
                    try {
                        out.append("expected syntax container before traversing ");
                        writeFriendlyIndex(out, i + 1);
                        out.append(" argument, had: ");
                        value.write(out);
                        out.append("\nArguments were:");
                        for (FusionValue arg : args)
                        {
                            out.append("\n  ");
                            FusionValue.write(out, arg);
                        }
                    }
                    catch (IOException ioe) {}
                    String message = out.toString();
                    throw contractFailure(message);
                }
            }
        }

        return value;
    }
}
