// Copyright (c) 2012-2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.falseBool;
import static com.amazon.fusion.FusionBool.isBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.trueBool;
import static com.amazon.fusion.FusionBool.unsafeBoolToJavaBoolean;
import static com.amazon.fusion.FusionNumber.isIntOrDecimal;
import static com.amazon.fusion.FusionNumber.unsafeNumberToBigDecimal;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.unsafeStringToJavaString;
import static com.amazon.fusion.FusionSymbol.isSymbol;
import static com.amazon.fusion.FusionSymbol.unsafeSymbolToJavaString;
import static com.amazon.fusion.FusionTimestamp.isTimestamp;
import static com.amazon.fusion.FusionTimestamp.unsafeTimestampToJavaTimestamp;
import static com.amazon.fusion.FusionUtils.safeEquals;
import com.amazon.fusion.FusionBool.BaseBool;
import com.amazon.fusion.FusionNumber.BaseNumber;
import com.amazon.ion.Timestamp;
import java.math.BigDecimal;

/**
 *
 */
final class FusionCompare
{
    private FusionCompare() {}



    static BaseBool isSame(Evaluator eval, Object left, Object right)
        throws FusionException
    {
        if (left == right) return trueBool(eval);

        if (left instanceof BaseNumber)
        {
            return ((BaseNumber) left).isSame(eval, right);
        }

        return falseBool(eval);
    }


    //========================================================================
    // Procedures

    enum EqualityTier
    {
        STRICT_EQUAL
        {
            @Override
            BaseBool eval(Evaluator eval, Object left, Object right)
                throws FusionException
            {
                return BaseValue.strictEquals(eval, left, right);
            }
        },
        TIGHT_EQUAL
        {
            @Override
            BaseBool eval(Evaluator eval, Object left, Object right)
                throws FusionException
            {
                return BaseValue.tightEquals(eval, left, right);
            }
        },
        LOOSE_EQUAL
        {
            @Override
            BaseBool eval(Evaluator eval, Object left, Object right)
                throws FusionException
            {
                return BaseValue.looseEquals(eval, left, right);
            }
        };

        abstract BaseBool eval(Evaluator eval, Object left, Object right)
            throws FusionException;
    }


    private static abstract class BaseCompareProc
        extends Procedure
    {
        BaseCompareProc(String doc, String... args)
        {
            super(doc, args);
        }


        FusionException failure(Object[] args)
        {
            return new ArgTypeFailure(this, "comparable types", -1, args);
        }


        abstract <T> boolean compare(Comparable<T> left,
                                     T             right,
                                     Object[]      args)
            throws FusionException;

        boolean compareBooleans(Boolean left, Boolean right, Object[] args)
            throws FusionException
        {
            throw failure(args);
        }

        boolean compareStrings(String left, String right, Object[] args)
            throws FusionException
        {
            throw failure(args);
        }


        @Override
        final Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Object arg0 = args[0];
            Object arg1 = args[1];

            if (isBool(eval, arg0) && isBool(eval, arg1))
            {
                Boolean left  = unsafeBoolToJavaBoolean(eval, arg0);
                Boolean right = unsafeBoolToJavaBoolean(eval, arg1);

                if (left != null && right != null)
                {
                    boolean r = compareBooleans(left, right, args);
                    return makeBool(eval, r);
                }
            }

            if (isIntOrDecimal(eval, arg0) && isIntOrDecimal(eval, arg1))
            {
                BigDecimal left  = unsafeNumberToBigDecimal(eval, arg0);
                BigDecimal right = unsafeNumberToBigDecimal(eval, arg1);

                if (left != null && right != null)
                {
                    boolean r = compare(left, right, args);
                    return makeBool(eval, r);
                }
            }

            if (isString(eval, arg0) && isString(eval, arg1))
            {
                String left  = unsafeStringToJavaString(eval, arg0);
                String right = unsafeStringToJavaString(eval, arg1);

                if (left != null && right != null)
                {
                    boolean r = compareStrings(left, right, args);
                    return makeBool(eval, r);
                }
            }

            if (isSymbol(eval, arg0) && isSymbol(eval, arg1))
            {
                String left  = unsafeSymbolToJavaString(eval, arg0);
                String right = unsafeSymbolToJavaString(eval, arg1);

                if (left != null && right != null)
                {
                    boolean r = compareStrings(left, right, args);
                    return makeBool(eval, r);
                }
            }

            if (isTimestamp(eval, arg0) && isTimestamp(eval, arg1))
            {
                Timestamp left  = unsafeTimestampToJavaTimestamp(eval, arg0);
                Timestamp right = unsafeTimestampToJavaTimestamp(eval, arg1);

                if (left != null && right != null)
                {
                    boolean r = compare(left, right, args);
                    return makeBool(eval, r);
                }
            }

            throw failure(args);
        }
    }


    static final class LessThanProc
        extends BaseCompareProc
    {
        LessThanProc()
        {
            //    "                                                                               |
            super("Documented in Fusion source", "a", "b");
        }

        @Override
        <T> boolean compare(Comparable<T> left, T right, Object[] args)
            throws FusionException
        {
            int r = left.compareTo(right);
            return (r < 0);
        }
    }


    static final class LessThanOrEqualToProc
        extends BaseCompareProc
    {
        LessThanOrEqualToProc()
        {
            //    "                                                                               |
            super("Documented in Fusion source", "a", "b");
        }

        @Override
        <T> boolean compare(Comparable<T> left, T right, Object[] args)
            throws FusionException
        {
            int r = left.compareTo(right);
            return (r <= 0);
        }
    }


    static final class GreaterThanProc
        extends BaseCompareProc
    {
        GreaterThanProc()
        {
            //    "                                                                               |
            super("Documented in Fusion source", "a", "b");
        }

        @Override
        <T> boolean compare(Comparable<T> left, T right, Object[] args)
            throws FusionException
        {
            int r = left.compareTo(right);
            return (r > 0);
        }
    }


    static final class GreaterThanOrEqualToProc
        extends BaseCompareProc
    {
        GreaterThanOrEqualToProc()
        {
            //    "                                                                               |
            super("Documented in Fusion source", "a", "b");
        }

        @Override
        <T> boolean compare(Comparable<T> left, T right, Object[] args)
            throws FusionException
        {
            int r = left.compareTo(right);
            return (r >= 0);
        }
    }


    static final class EqualProc
        extends BaseCompareProc
    {
        EqualProc()
        {
            //    "                                                                               |
            super("Returns true if the parameters are equivalent, depending on their types.  Int\n" +
                  "and decimal values may be mixed and are compared without regard to precision.\n" +
                  "Timestamps are compared to each other without regard to precision.  String and\n" +
                  "symbol values are compared by character. Bool values may also be compared.\n" +
                  "Annotations are ignored.",
                  "a", "b");
        }

        @Override
        boolean compareBooleans(Boolean left, Boolean right, Object[] args)
            throws FusionException
        {
            // Boolean instances are interned so identity equality is correct.
            return left == right;
        }

        @Override
        boolean compareStrings(String left, String right, Object[] args)
            throws FusionException
        {
            return safeEquals(left, right);
        }

        @Override
        <T> boolean compare(Comparable<T> left, T right, Object[] args)
            throws FusionException
        {
            int r = left.compareTo(right);
            return (r == 0);
        }
    }


    static final class IsIdenticalProc
        extends Procedure
    {
        IsIdenticalProc()
        {
            //    "                                                                               |
            super("Docs in Fusion source",
                  "left", "right");
        }

        @Override
        final BaseBool doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Object arg0 = args[0];
            Object arg1 = args[1];

            return makeBool(eval, arg0 == arg1);
        }
    }


    static final class IsSameProc
        extends Procedure
    {
        IsSameProc()
        {
            //    "                                                                               |
            super("Docs in Fusion source",
                  "left", "right");
        }

        @Override
        final BaseBool doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Object arg0 = args[0];
            Object arg1 = args[1];

            return isSame(eval, arg0, arg1);
        }
    }


    static final class LooseEqualProc
        extends Procedure
    {
        LooseEqualProc()
        {
            //    "                                                                               |
            super("Docs in Fusion source",
                  "left", "right");
        }

        @Override
        final BaseBool doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Object arg0 = args[0];
            Object arg1 = args[1];

            return BaseValue.looseEquals(eval, arg0, arg1);
        }
    }


    static final class TightEqualProc
        extends Procedure
    {
        TightEqualProc()
        {
            //    "                                                                               |
            super("Docs in Fusion source",
                  "left", "right");
        }

        @Override
        final BaseBool doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Object arg0 = args[0];
            Object arg1 = args[1];

            return BaseValue.tightEquals(eval, arg0, arg1);
        }
    }


    static final class StrictEqualProc
        extends Procedure
    {
        StrictEqualProc()
        {
            //    "                                                                               |
            super("Docs in Fusion source",
                  "left", "right");
        }

        @Override
        final BaseBool doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

            Object arg0 = args[0];
            Object arg1 = args[1];

            return BaseValue.strictEquals(eval, arg0, arg1);
        }
    }
}
