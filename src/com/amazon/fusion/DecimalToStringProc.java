// Copyright (c) 2014 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import static com.amazon.fusion.FusionNumber.checkNullableDecimalArg;
import static com.amazon.fusion.FusionString.makeString;
import java.math.BigDecimal;
import java.io.IOException;

import com.amazon.ion.IonValue;
import com.amazon.ion.IonWriter;
import com.amazon.ion.Decimal;

final class DecimalToStringProc
    extends Procedure
{
    DecimalToStringProc()
    {
        super("See documentation in /fusion/experimental/decimal.fusion",
              "decimal");
    }

    @Override
    Object doApply(Evaluator eval, Object[] args)
        throws FusionException
    {
        checkArityExact(1, args);

        BigDecimal val = checkNullableDecimalArg(eval, this, 0, args);
        String text = null;
        if(val != null) {
            StringBuilder outputBuilder = new StringBuilder();
            IonWriter iw = WRITER_BUILDER.build(outputBuilder);

            try {
                iw.writeDecimal(val);
                iw.finish();
                text = outputBuilder.toString();
            }
            catch(IOException e) {
                throw contractFailure("Failed writing Ion value.", e);
            }
        }
        return makeString(eval, text);
    }
}
