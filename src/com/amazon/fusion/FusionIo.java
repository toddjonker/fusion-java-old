// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 *
 */
final class FusionIo
{
    private FusionIo() {}


    /** The singular {@code eof} value. */
    private final static FusionValue EOF =
        new FusionValue()
        {
            @Override
            void write(Evaluator eval, Appendable out) throws IOException
            {
                out.append("{{{eof}}}");
            }
        };


    static Object eof(Evaluator eval)
    {
        return EOF;
    }


    static boolean isEof(Evaluator eval, Object v)
    {
        assert eval != null;
        return (v == EOF);
    }


    static final class IsEofProc
        extends Procedure1
    {
        IsEofProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is the Fusion end-of-file value.  This value is\n"
                + "bound to the name `eof`.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
        {
            return eval.newBool(arg == EOF);
        }
    }
}

