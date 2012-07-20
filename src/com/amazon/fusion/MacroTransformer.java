// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;


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
    FusionValue invoke(Evaluator eval, Environment env, SyntaxSexp expr)
        throws FusionException
    {
        Object resultFv = eval.applyNonTail(myTransformer, expr);
        // TODO error check
        return eval.bounceTailExpression(env, resultFv);
    }
}
