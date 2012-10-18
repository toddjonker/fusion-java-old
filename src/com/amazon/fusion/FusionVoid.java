// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


/**
 * Utilities for working with Fusion's singular {@code void} value.
 */
public final class FusionVoid
{
    private FusionVoid() {}


    /** The singular {@code void} value. */
    private final static FusionValue VOID = FusionValue.UNDEF;
    // TODO FUSION-71 this shouldn't be UNDEF.


    //========================================================================
    // Constructors

    /**
     * Returns the Fusion {@code void} value.
     * <b>It is an error to make any assumptions about whether this value is
     * null or not!</b>
     */
    static Object voidValue(Evaluator eval)
        throws FusionException
    {
        return VOID;
    }


    //========================================================================
    // Predicates

    @Deprecated
    public static boolean isVoid(Object v)
        throws FusionException
    {
        return (v == VOID);
    }

    static boolean isVoid(Evaluator eval, Object v)
        throws FusionException
    {
        return (v == VOID);
    }


    //========================================================================


    static final class VoidProc extends Procedure
    {
        VoidProc()
        {
            //    "                                                                               |
            super("Returns the Fusion void value, ignoring all ARGs.",
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
            super("Determines whether a VALUE is the Fusion void value.",
                  "value");
        }

        @Override
        Object doApply(Evaluator eval, Object arg)
        {
            return eval.newBool(arg == VOID);
        }
    }
}
