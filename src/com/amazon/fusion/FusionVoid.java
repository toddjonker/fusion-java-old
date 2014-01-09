// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import com.amazon.fusion.FusionBool.BaseBool;
import java.io.IOException;


/**
 * Utilities for working with Fusion's singular {@code void} value.
 *
 * @see FusionValue
 */
public final class FusionVoid
{
    private FusionVoid() {}


    /** The singular {@code void} value. */
    private final static BaseValue VOID =
        new BaseValue()
        {
            @Override
            BaseBool isTruthy(Evaluator eval)
            {
                return falseBool(eval);
            }

            @Override
            BaseBool not(Evaluator eval)
            {
                return trueBool(eval);
            }

            @Override
            BaseBool looseEquals(Evaluator eval, Object right)
                throws FusionException
            {
                // Object comparison has already been performed, so we know
                // we've not been given void.
                return falseBool(eval);
            }

            @Override
            void write(Evaluator eval, Appendable out) throws IOException
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
            return makeBool(eval, arg == VOID);
        }
    }
}
