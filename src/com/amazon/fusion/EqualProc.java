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

final class EqualProc
    extends Procedure
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

    private static final String EXPECTATION =
        "non-null bool, int, decimal, string, symbol, or timestamp";


    private FusionException failure(Object[] args)
    {
        return new ArgTypeFailure(this, EXPECTATION, -1, args);
    }

    private int compare(IonValue leftVal, IonValue rightVal, Object[] args)
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


    private boolean equals(IonValue leftVal, IonValue rightVal, Object[] args)
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
                // Bool is checked last since it's least likely to be used.
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
                return compare(leftVal, rightVal, args) == 0;
            }
        }

        throw failure(args);
    }


    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        IonValue leftVal  = FusionValue.castToIonValueMaybe(args[0]);
        IonValue rightVal = FusionValue.castToIonValueMaybe(args[1]);

        if (leftVal  == null || leftVal.isNullValue() ||
            rightVal == null || rightVal.isNullValue())
        {
            throw new ArgTypeFailure(this, EXPECTATION, -1, args);
        }

        boolean r = equals(leftVal, rightVal, args);
        return eval.newBool(r);
    }
}
