// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonBool;
import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonString;
import com.amazon.ion.IonSymbol;
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
            return new ArgTypeFailure(this, "values are not comparable", -1,
                                      args);
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


        @Override
        final Object doApply(Evaluator eval, Object[] args)
            throws FusionException
        {
            checkArityExact(args);

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
            switch (leftVal.getType())
            {
                case STRING:
                {
                    if (rightVal instanceof IonString)
                    {
                        IonString left  = (IonString) leftVal;
                        IonString right = (IonString) rightVal;
                        return left.stringValue().equals(right.stringValue());
                    }
                    break;
                }
                case SYMBOL:
                {
                    if (rightVal instanceof IonSymbol)
                    {
                        IonSymbol left  = (IonSymbol) leftVal;
                        IonSymbol right = (IonSymbol) rightVal;
                        return left.stringValue().equals(right.stringValue());
                    }
                    break;
                }
                case BOOL:
                {
                    if (rightVal instanceof IonBool)
                    {
                        IonBool left  = (IonBool) leftVal;
                        IonBool right = (IonBool) rightVal;
                        return left.booleanValue() == right.booleanValue();
                    }
                    break;
                }
                default:
                {
                    return compareNumbers(leftVal, rightVal, args) == 0;
                }
            }

            throw failure(args);
        }
    }
}
