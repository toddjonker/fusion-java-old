// Copyright (c) 2012-2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonDecimal;
import com.amazon.ion.IonInt;
import com.amazon.ion.IonTimestamp;
import com.amazon.ion.IonValue;
import com.amazon.ion.Timestamp;
import java.math.BigDecimal;
import java.math.BigInteger;

final class LessThanProc
    extends Procedure
{
    LessThanProc()
    {
        //    "                                                                               |
        super("Returns true if `a` is less than `b`.  Int and decimal values may be mixed and\n" +
              "are compared without regard to precision.  Timestamps are compared to each\n" +
              "other without regard to precision.  Annotations are ignored.",
              "a", "b");
    }

    private static final String EXPECTATION =
        "non-null int, decimal, or timestamp";


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
            throw failure(args);
        }

        int r = compare(leftVal, rightVal, args);
        return eval.newBool(r < 0);
    }
}
