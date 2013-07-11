// Copyright (c) 2013 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import java.math.BigDecimal;
import java.math.MathContext;


/** EXPERIMENTAL **/
final class DecimalDivideProc
    extends DivideProc
{
    @Override
    BigDecimal divide(Evaluator eval, Object[] args,
                      BigDecimal dividend, BigDecimal divisor)
        throws FusionException
    {
        return dividend.divide(divisor, MathContext.DECIMAL128);
    }
}
