// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionUtils.writeFriendlyIndex;
import com.amazon.ion.IonContainer;
import com.amazon.ion.IonSequence;
import com.amazon.ion.IonStruct;
import com.amazon.ion.IonValue;
import java.io.IOException;

final class DotProc
    extends Procedure
{
    DotProc()
    {
        //    "                                                                               |
        super("Traverses down through an Ion data structure.\n" +
              "CONTAINER must be Ion container (struct, list, or sexp).\n" +
              "Each PART must be a string, symbol, or int, to denote either a struct's\n" +
              "field-name or a sequence's index.",
              "container", "part", DOTDOTDOT);
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityAtLeast(1, args);
        IonContainer c = checkContainerArg(0, args);
        IonValue value = c;

        final int lastArg = args.length - 1;
        for (int i = 1; i <= lastArg; i++)
        {
            switch (c.getType())
            {
                case LIST:
                case SEXP:
                {
                    long index = checkLongArg(i, args);
                    if (c.size() <= index)
                    {
                        return UNDEF;
                    }
                    IonSequence s = (IonSequence) c;
                    value = s.get((int) index);
                    break;
                }
                case STRUCT:
                {
                    String field = checkTextArg(i, args);
                    IonStruct s = (IonStruct) c;
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
                    c = (IonContainer) value;
                }
                catch (ClassCastException cce)
                {
                    StringBuilder out = new StringBuilder();
                    try {
                        out.append("expected container before traversing ");
                        writeFriendlyIndex(out, i + 1);
                        out.append(" argument, had: ");
                        FusionUtils.writeIon(out, value);
                        out.append("\nArguments were:");
                        for (Object arg : args)
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

        return new DomValue(value);
    }

}
