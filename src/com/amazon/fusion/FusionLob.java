// Copyright (c) 2013-2015 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionString.makeString;
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

        @Override
        final boolean isAnnotatable()
        {
            return true;
        }

        byte[] bytesNoCopy()
        {
            return null;
        }

        byte[] bytesCopy()
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
        SyntaxValue datumToSyntaxMaybe(Evaluator eval, SourceLocation loc)
        {
            return makeSyntax(eval, loc, this);
        }
    }


    //========================================================================
    // Constructors


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


    /** NOT FOR PUBLIC CONSUMPTION */
    static byte[] unsafeLobBytesNoCopy(Evaluator eval, Object lob)
    {
        return ((BaseLob) lob).bytesNoCopy();
    }


    /**
     * Returns the contents of a lob as a byte array, without making extra
     * copies. <b>The caller must take great care not to modify the content of
     * the array!</b> Any changes will violate the lob's immutability contract,
     * which may cause terrible problems.
     *
     * @param lob must be a Fusion blob or clob.
     *
     * @return null if {@code lob} is {@code null.blob} or {@code null.clob},
     * otherwise a byte array of the data within the lob.
     */
    public static byte[] unsafeLobBytesNoCopy(TopLevel top, Object lob)
    {
        return ((BaseLob) lob).bytesNoCopy();
    }


    /**
     * Extracts the contents of a lob into a byte array. The bytes are copied
     * from the lob to prevent modification to the value.
     *
     * @param lob must be a Fusion blob or clob.
     *
     * @return null if {@code lob} is {@code null.blob} or {@code null.clob},
     * otherwise a fresh copy of the data within the lob.
     */
    public static byte[] unsafeLobBytesCopy(TopLevel top, Object lob)
    {
        return ((BaseLob) lob).bytesCopy();
    }


    //========================================================================
    // Procedure Helpers

    public static class UnsafeLobToHexProc
        extends Procedure
    {
        private static final char[] DIGITS = {
            '0', '1', '2', '3', '4', '5', '6', '7',
            '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'
        };

        private static final int[] DIGIT_MASKS = { 0xF0, 0x0F };

        @Override
        Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            byte[] in = unsafeLobBytesNoCopy(eval, args[0]);
            StringBuilder out = new StringBuilder(in.length * 2);
            for (byte b : in)
            {
                out.append(DIGITS[(DIGIT_MASKS[0] & b) >>> 4]);
                out.append(DIGITS[(DIGIT_MASKS[1] & b)]);
            }

            return makeString(eval, out.toString());
        }
    }
}
