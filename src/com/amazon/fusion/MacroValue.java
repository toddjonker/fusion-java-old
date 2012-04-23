// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.ion.IonSexp;
import com.amazon.ion.IonValue;

/**
 * Base class for Fusion macros, performing syntax expansion.
 */
abstract class MacroValue
    extends KeywordValue
{
    MacroValue(String bodyPattern, String doc)
    {
        super(bodyPattern, doc);
    }


    /**
     * Performs a single "level" of macro expansion.
     *
     * @param env
     * @param expr the input expression, including the keyword symbol.
     * @return
     */
    abstract IonValue expand(IonSexp expr)
        throws SyntaxFailure;


    @Override
    final FusionValue invoke(Evaluator eval, Environment env, IonSexp expr)
        throws FusionException
    {
        IonValue expanded = expand(expr);
        return eval.bounceTailExpression(env, expanded);
    }
}
