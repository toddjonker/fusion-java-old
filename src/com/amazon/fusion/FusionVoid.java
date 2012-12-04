// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.io.IOException;


/**
 * Utilities for working with Fusion's singular {@code void} value.
 */
public final class FusionVoid
{
    private FusionVoid() {}


    /** The singular {@code void} value. */
    private final static FusionValue VOID =
        new FusionValue()
        {
            @Override
            void write(Appendable out) throws IOException
            {
                out.append("{{{void}}}");
            }
        };


    //========================================================================
    // Constructors

    /**
     * Returns the Fusion {@code void} value.
     * <b>It is an error to make any assumptions about whether this value is
     * null or not!</b>
     */
    static Object voidValue(Evaluator eval)
    {
        return VOID;
    }


    //========================================================================
    // Predicates

    public static boolean isVoid(TopLevel top, Object v)
    {
        assert top != null;
        return (v == VOID);
    }

    static boolean isVoid(Evaluator eval, Object v)
    {
        assert eval != null;
        return (v == VOID);
    }


    //========================================================================


    static final class VoidProc extends Procedure
    {
        VoidProc()
        {
            //    "                                                                               |
            super("Returns the singular void value, ignoring all `arg`s.",
                  "arg", DOTDOTDOT);
        }

        @Override
        Object doApply(Evaluator eval, Object[] args)
        {
            return VOID;
        }
    }


    static final class IsVoidProc extends Procedure1
    {
        IsVoidProc()
        {
            //    "                                                                               |
            super("Determines whether a `value` is the Fusion void value.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
        {
            return eval.newBool(arg == VOID);
        }
    }
}
