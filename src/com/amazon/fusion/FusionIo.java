// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;

/**
 * Utilities for input and output of Fusion data.
 */
public final class FusionIo
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


    /**
     * <a href="{@docRoot}/../fusion/io.html#write">Writes</a> a text representation of a Fusion value, following Ion syntax
     * where possible, including for strings.
     * The result will be unreadable (by the Fusion and Ion readers) if the
     * value contains any non-Ionizable data (void, closures, etc.).
     *
     * @param top must not be null.
     * @param value must not be null.
     * @param out the output stream; not null.
     * @throws FusionException if there's an exception thrown by the output
     * stream.
     */
    @SuppressWarnings("deprecation")
    public static void write(TopLevel top, Object value, Appendable out)
        throws FusionException
    {
        FusionWrite.write(((StandardTopLevel) top).getEvaluator(), out, value);
    }

}

