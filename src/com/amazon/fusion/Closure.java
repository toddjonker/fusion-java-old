// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.LocalEnvironment.LexicalBinding;

/**
 * A user-defined procedure, the result of evaluating a {@link LambdaKeyword}.
 */
final class Closure
    extends Procedure
{
    // TODO FUSION-48 Don't retain Bindings, they retain too much data.
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
    Closure(Store enclosure, String doc, String[] argNames,
            LexicalBinding[] params,
            CompiledForm body)
    {
        super(doc, argNames);

        myEnclosure = enclosure;
        myParams    = params;
        myBody      = body;
    }


    @Override
    FusionValue invoke(Evaluator eval, final FusionValue[] args)
        throws FusionException
    {
        checkArityExact(args);

        // TODO FUSION-49 separate Store from Environment
        Environment bodyEnv =
            new LocalEnvironment((Environment) myEnclosure, myParams, args);

        return eval.bounceTailForm(bodyEnv, myBody);
    }
}
