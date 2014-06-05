// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.makeDecimal;
import static com.amazon.fusion.FusionString.checkNullableStringArg;
import com.amazon.ion.Decimal;
import java.math.BigDecimal;

final class StringToDecimalProc
    extends Procedure
{
    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        String val = checkNullableStringArg(eval, this, 0, args);

        BigDecimal bigDecimal = (val != null ? parse(val, args) : null);

        return makeDecimal(eval, bigDecimal);
    }


    BigDecimal parse(String text, Object[] args)
        throws ArgumentException
    {
        // Note: This implementation is experimental! It relies on java's BigDecimal
        // API to perform string -> decimal parsing. This means it will NOT handle the
        // exponent notation described for IonDecimal in the Ion spec:
        // (https://amazon-ion.github.io/ion-docs/docs/spec.html#real-numbers)
        // It also WILL (incorrectly) accept the exponent notation defined for BigDecimal:
        // (http://download.java.net/jdk7/archive/b123/docs/api/java/math/BigDecimal.html#BigDecimal%28java.lang.String%29)
        try
        {
            // Work-around behavior of BigDecimal constructor, which
            // accepts leading '+' (which we don't want).
            if (! text.startsWith("+"))
            {
                return Decimal.valueOf(text);
            }
        }
        catch (NumberFormatException e) { }

        throw argFailure("valid decimal content", 0, args);
    }
}
