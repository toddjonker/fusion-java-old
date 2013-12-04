// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionBool.isBool;
import static com.amazon.fusion.FusionBool.makeBool;
import static com.amazon.fusion.FusionBool.unsafeBoolToJavaBoolean;
import static com.amazon.fusion.FusionString.isString;
import static com.amazon.fusion.FusionString.unsafeStringToJavaString;
import static com.amazon.fusion.FusionSymbol.isSymbol;
import static com.amazon.fusion.FusionSymbol.unsafeSymbolToJavaString;
import static com.amazon.fusion.FusionTimestamp.isTimestamp;
import static com.amazon.fusion.FusionTimestamp.unsafeTimestampToJavaTimestamp;
import static com.amazon.fusion.FusionUtils.safeEquals;
import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 *
 */
final class FusionCompare
{
    private FusionCompare() {}


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


        int compareNumbers(IonValue leftVal, IonValue rightVal, Object[] args)
            throws FusionException
        {
            switch (leftVal.getType())
            {
                case INT:
                {
                    BigInteger lInt = ((IonInt) leftVal).bigIntegerValue();

                    switch (rightVal.getType())
                    {
                        case INT:
                        {
                            BigInteger rInt = ((IonInt) rightVal).bigIntegerValue();
                            return lInt.compareTo(rInt);
                        }
                        case DECIMAL:
                        {
                            BigDecimal lDec = new BigDecimal(lInt);
                            BigDecimal rDec = ((IonDecimal) rightVal).bigDecimalValue();
                            return lDec.compareTo(rDec);
                        }
                    }
                    break;
                }
                case DECIMAL:
                {
                    BigDecimal lDec = ((IonDecimal) leftVal).bigDecimalValue();

                    switch (rightVal.getType())
                    {
                        case INT:
                        {
                            BigInteger rInt = ((IonInt) rightVal).bigIntegerValue();
                            BigDecimal rDec = new BigDecimal(rInt);
                            return lDec.compareTo(rDec);
                        }
                        case DECIMAL:
                        {
                            BigDecimal rDec = ((IonDecimal) rightVal).bigDecimalValue();
                            return lDec.compareTo(rDec);
                        }
                    }
                    break;
                }
                case TIMESTAMP:
                {
                    Timestamp lTime = ((IonTimestamp) leftVal).timestampValue();

                    switch (rightVal.getType())
                    {
                        case TIMESTAMP:
                        {
                            Timestamp rTime = ((IonTimestamp) rightVal).timestampValue();
                            return lTime.compareTo(rTime);
                        }
                    }
                    break;
                }
            }

            throw failure(args);
        }


        abstract boolean compare(IonValue leftVal, IonValue rightVal,
                                 Object[] args)
            throws FusionException;


        boolean compareBooleans(Boolean left, Boolean right, Object[] args)
            throws FusionException
        {
            throw failure(args);
        }

        boolean compareTimestamps(Timestamp left, Timestamp right, Object[] args)
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
                    boolean r = compareTimestamps(left, right, args);
                    return makeBool(eval, r);
                }
            }


            IonValue leftVal  = FusionValue.castToIonValueMaybe(args[0]);
            IonValue rightVal = FusionValue.castToIonValueMaybe(args[1]);

            if (leftVal  == null || leftVal.isNullValue() ||
                rightVal == null || rightVal.isNullValue())
            {
                throw failure(args);
            }

            boolean r = compare(leftVal, rightVal, args);
            return eval.newBool(r);
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
        boolean compare(IonValue leftVal, IonValue rightVal, Object[] args)
            throws FusionException
        {
            int r = compareNumbers(leftVal, rightVal, args);
            return (r < 0);
        }

        @Override
        boolean compareTimestamps(Timestamp left, Timestamp right, Object[] args)
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
        boolean compare(IonValue leftVal, IonValue rightVal, Object[] args)
            throws FusionException
        {
            int r = compareNumbers(leftVal, rightVal, args);
            return (r <= 0);
        }

        @Override
        boolean compareTimestamps(Timestamp left, Timestamp right, Object[] args)
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
        boolean compare(IonValue leftVal, IonValue rightVal, Object[] args)
            throws FusionException
        {
            int r = compareNumbers(leftVal, rightVal, args);
            return (r > 0);
        }

        @Override
        boolean compareTimestamps(Timestamp left, Timestamp right, Object[] args)
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
        boolean compare(IonValue leftVal, IonValue rightVal, Object[] args)
            throws FusionException
        {
            int r = compareNumbers(leftVal, rightVal, args);
            return (r >= 0);
        }

        @Override
        boolean compareTimestamps(Timestamp left, Timestamp right, Object[] args)
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
        boolean compare(IonValue leftVal, IonValue rightVal, Object[] args)
            throws FusionException
        {
            return compareNumbers(leftVal, rightVal, args) == 0;
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
        boolean compareTimestamps(Timestamp left, Timestamp right, Object[] args)
            throws FusionException
        {
            int r = left.compareTo(right);
            return (r == 0);
        }
    }
}
