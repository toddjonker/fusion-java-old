// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 *
 */
final class MacroTransformer
    extends KeywordValue
{
    private final Procedure myTransformer;


    MacroTransformer(Procedure transformer)
    {
        super(null, null); // TODO
        myTransformer = transformer;
    }


    @Override
    FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        DomValue exprValue = new DomValue(expr);
        FusionValue resultFv = eval.applyNonTail(myTransformer, exprValue);
        IonValue resultIv = resultFv.ionValue();
        return eval.bounceTailExpression(env, resultIv);
    }
}
