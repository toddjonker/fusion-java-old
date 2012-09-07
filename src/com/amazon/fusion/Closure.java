// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ArityFailure.Variability;
import com.amazon.fusion.LocalEnvironment.LexicalBinding;

/**
 * A user-defined procedure, the result of evaluating a {@link LambdaKeyword}.
 */
final class Closure
    extends Procedure
{
    private final Store            myEnclosure;
    private final LexicalBinding[] myParams;
    private final CompiledForm     myBody;


    /**
     * Constructs a new closure from its source and enclosing lexical
     * environment.
     *
     * @param enclosure the store lexically surrounding the source of this
     *  closure.  Any free variables in the procedure are expected to be bound
     *  here.
     */
    Closure(Store enclosure, String doc, LexicalBinding[] params,
            CompiledForm body)
    {
        // Don't pre-compute param names every time we create a closure.
        // Instead we extract them on-demand by overriding getParamNames().
        super(doc, FusionUtils.EMPTY_STRING_ARRAY);

        myEnclosure = enclosure;
        myParams    = params;
        myBody      = body;
    }


    @Override
    String[] getParamNames()
    {
        return LexicalBinding.toNames(myParams);
    }


    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
        throws FusionException
    {
        final int paramCount = myParams.length;
        if (paramCount != args.length)
        {
            throw new ArityFailure(this, paramCount, Variability.EXACT, args);
        }

        // TODO remove cast
        Environment bodyEnv =
            new LocalEnvironment((Environment) myEnclosure, myParams, args);

        return eval.bounceTailForm(bodyEnv, myBody);
    }
}
