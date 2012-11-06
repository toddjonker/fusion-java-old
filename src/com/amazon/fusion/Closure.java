// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

/**
 * A user-defined procedure, the result of evaluating a {@link LambdaForm}.
 */
final class Closure
    extends Procedure
{
    private final Store        myEnclosure;
    private final CompiledForm myBody;


    /**
     * Constructs a new closure from its source and enclosing lexical
     * environment.
     *
     * @param enclosure the store lexically surrounding the source of this
     *  closure.  Any free variables in the procedure are expected to be bound
     *  here.
     */
    Closure(Store enclosure, String doc, String[] argNames, CompiledForm body)
    {
        super(doc, argNames);

        myEnclosure = enclosure;
        myBody      = body;
    }


    @Override
    Object doApply(Evaluator eval, final Object[] args)
        throws FusionException
    {
        checkArityExact(args);

        Store localStore = new LocalStore(myEnclosure, args);

        return eval.bounceTailForm(localStore, myBody);
    }
}
