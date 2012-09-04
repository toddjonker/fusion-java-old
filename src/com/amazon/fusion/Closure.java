// Copyright (c) 2012 Amazon.com, Inc.  All rights reserved.

package com.amazon.fusion;

import com.amazon.fusion.ArityFailure.Variability;

/**
 * A user-defined procedure, the result of evaluating a {@link LambdaKeyword}.
 */
final class Closure
    extends Procedure
{
    private final Environment myEnclosure;
    private final SyntaxSymbol[] myParams;
    private final SyntaxSexp myDefinition;

    /**
     * Index within {@link #myDefinition} of the first body form.
     */
    private final int myBodyStart;

    /**
     * Constructs a new closure from its source and enclosing lexical
     * environment.
     *
     * @param enclosure the lexical environment surrounding the source of this
     *  closure.  Any free variables in the procedure are expected to be bound
     *  here.
     * @param definition the source text of the {@code lambda} expression.
     */
    Closure(Environment enclosure, SyntaxSexp definition, String doc,
            SyntaxSymbol[] params, int bodyStartIndex)
    {
        super(doc, SyntaxSymbol.toNames(params));

        myEnclosure = enclosure;
        myParams = params;
        myDefinition = definition;
        myBodyStart = bodyStartIndex;
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

        Environment bodyEnv;
        if (paramCount == 0)
        {
            bodyEnv = myEnclosure;
        }
        else
        {
            bodyEnv = new LocalEnvironment(myEnclosure, myParams, args);
        }

        FusionValue result;
        final int bodyEnd = myDefinition.size() - 1;
        for (int i = myBodyStart; i < bodyEnd; i++)
        {
            SyntaxValue expr = myDefinition.get(i);
            result = eval.eval(bodyEnv, expr);
        }

        SyntaxValue expr = myDefinition.get(bodyEnd);

        result = eval.bounceTailExpression(bodyEnv, expr);

        return result;
    }

}
