// Copyright (c) 2013-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.SimpleSyntaxValue.makeSyntax;
import com.amazon.fusion.FusionBool.BaseBool;
import java.util.Arrays;


/**
 * Utilities for working with Fusion {@code blob} and {@code clob} values.
 *
 * @see FusionValue
 */
public final class FusionLob
{
    private FusionLob() {}


    //========================================================================
    // Representation Classes


    abstract static class BaseLob
        extends BaseValue
    {
        BaseLob() {}

        byte[] bytesNoCopy()
        {
            return null;
        }

        static BaseBool actualLobEquals(Evaluator eval,
                                        byte[]    leftBytes,
                                        Object    rightLob)
        {
            byte[] rightBytes = ((BaseLob) rightLob).bytesNoCopy();
            boolean b = Arrays.equals(leftBytes, rightBytes);
            return makeBool(eval, b);
        }

        @Override
        BaseBool looseEquals(Evaluator eval, Object right)
            throws FusionException
        {
            if (right instanceof BaseLob)
            {
                byte[] leftBytes = bytesNoCopy();
                return actualLobEquals(eval, leftBytes, right);
            }
            return falseBool(eval);
        }

        @Override
        SyntaxValue toStrippedSyntaxMaybe(Evaluator eval)
        {
            return makeSyntax(eval, /*location*/ null, this);
        }
    }


    //========================================================================
    // Predicates


    /**
     * Determines whether a Fusion value has type {@code blob} or {@code clob}.
     */
    public static boolean isLob(TopLevel top, Object value)
        throws FusionException
    {
        return (value instanceof BaseLob);
    }


    static boolean isLob(Evaluator eval, Object value)
        throws FusionException
    {
        return (value instanceof BaseLob);
    }


    //========================================================================
    // Conversions


    //========================================================================
    // Procedure Helpers

}
